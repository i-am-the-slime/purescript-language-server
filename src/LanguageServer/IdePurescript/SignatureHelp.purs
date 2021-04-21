module LanguageServer.IdePurescript.SignatureHelp where

import Prelude

import Control.Alt ((<|>))
import Control.Error.Util (note)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.MonadZero as MZ
import Data.Argonaut (stringify)
import Data.Array (foldl)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens (Prism', Traversal', prism', (%~), (.~), (^.), (^?))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Utils (endsWith)
import Data.Symbol (SProxy(..))
import Data.Tree (mkTree)
import Data.Tree.Zipper as Zipper
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Object (Object)
import IdePurescript.Modules (Module, getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getTypeInfo)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (Offsets, ParameterInformation, SignatureHelp(..), SignatureHelpParams, mkOffsets)
import LanguageServer.IdePurescript.SignatureHelp.Types (State)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (DocumentStore, Position(..), Range(..), Settings, character, endPosition, markupContent, uri)
import LanguageServer.Window (showError)
import PscIde.Command as C
import PureScript.CST (RecoveredParserResult(..), parseExpr, parseType)
import PureScript.CST.Errors (ParseError(..), printParseError)
import PureScript.CST.Print (printSourceToken)
import PureScript.CST.Range (class TokensOf, rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Castable (cast)
import Untagged.Union (uorToMaybe)


activeParameter ∷ Traversal' SignatureHelp Int
activeParameter =
  _Newtype
    <<< prop (SProxy ∷ _ "activeParameter")
    <<< activeParameter_

activeParameter_ ∷ Prism' (Nullable Int) Int
activeParameter_ = prism' Nullable.notNull Nullable.toMaybe

debugHelp ∷ String -> SignatureHelp
debugHelp label =
  SignatureHelp
    { activeParameter: Nullable.null
    , activeSignature: Nullable.null
    , signatures: NonEmptyArray.singleton (cast { label })
    }

requireMaybe ∷ Maybe ~> MaybeT Aff
requireMaybe = MaybeT <<< pure

orElse ∷ ∀ err. err -> Maybe ~> ExceptT err Aff
orElse err maybe = (ExceptT <<< pure) (note err maybe)

mkGetSignatureHelp :: Effect (DocumentStore -> Foreign -> ServerState -> SignatureHelpParams -> Aff (Nullable SignatureHelp))
mkGetSignatureHelp = Ref.new Nothing <#> getSignatureHelp

getSignatureHelp ∷ Ref (Maybe State) -> DocumentStore -> Settings -> ServerState -> SignatureHelpParams -> Aff (Nullable SignatureHelp)
getSignatureHelp stateRef docs settings (ServerState serverState) params = do
  result <- runExceptT go
  liftEffect case serverState.connection, result of
    Nothing, _ -> pure Nullable.null
    Just connection, Left err -> showError connection err $> Nullable.null
    _, Right res -> pure (Nullable.notNull res)
  where
  go ∷ ExceptT String Aff SignatureHelp
  go = do
    port <- serverState.port # orElse "No Port"
    let modules = serverState.modules
    let connection = serverState.connection
    context@{ triggerKind } <- params.context # uorToMaybe # orElse "No context"
    let helpIsActive = fromMaybe false (uorToMaybe context.activeSignatureHelp)
    if not helpIsActive then do
      startSignatureHelp stateRef docs port modules params
    else
      continueSignatureHelp stateRef docs port modules params

continueSignatureHelp ∷ Ref (Maybe State) -> DocumentStore -> Int -> _ -> SignatureHelpParams -> ExceptT String Aff SignatureHelp
continueSignatureHelp stateRef docs port modules params@{ textDocument } = do
  storedLoc <- Ref.read stateRef # liftEffect >>= orElse "no previous state"
  let stored = Zipper.value storedLoc
  let end = max params.position stored.maxPosition
  maybeDoc <- getDocument docs (uri textDocument) # liftEffect
  case maybeDoc of 
    Nothing -> throwError "document not found"
    Just doc -> do
      text <- getTextAtRange doc (Range { start: stored.startPosition, end }) # liftEffect
      let relevantText = text
      rangeIndex <- findArgumentIndex params.position storedLoc relevantText
      inRangeIndex <- MZ.guard (rangeIndex < NonEmptyArray.length stored.parameters) $> rangeIndex
      pure (stored.previousResponse # activeParameter .~ inRangeIndex)
  where
  dropSuffix p s = String.stripSuffix p s # fromMaybe s

findArgumentIndex ∷ Position -> State -> String -> ExceptT String Aff Int
findArgumentIndex paramsPosition storedLoc relevantText = do
  let
    stored = Zipper.value storedLoc
    functionArgumentIndex = findInFunctionApplication paramsPosition stored.startPosition relevantText
    first = firstArgument stored.functionName relevantText
    previous = fallBackToPreviousIndex storedLoc
  first <|> functionArgumentIndex <|> previous

fallBackToPreviousIndex :: State -> ExceptT String Aff Int
fallBackToPreviousIndex stored =
  (Zipper.value stored).previousResponse ^? activeParameter
    # note "No previous to fallback to"
    # pure
    # ExceptT

firstArgument ∷ String -> String -> ExceptT String Aff Int
firstArgument storedFunctionName relevantText = do
  if isLonger && isTheSameModuloWhitespace then
    pure 0
  else
    throwError "Not first index."
  where
  isLonger = String.length relevantText > String.length storedFunctionName

  isTheSameModuloWhitespace = String.trim relevantText == storedFunctionName

findInFunctionApplication :: Position -> Position -> String -> ExceptT String Aff Int
findInFunctionApplication paramsPosition storedStartPosition relevantText = do
  findIt relevantText
  where
  findIt text = do
    relativeRanges <- getExpression text # pure # ExceptT
    let absoluteRanges = toAbsoluteRange storedStartPosition <$> relativeRanges
    let lastIndex = NonEmptyArray.length absoluteRanges
    let lastEndPosition = (NonEmptyArray.last absoluteRanges) ^. endPosition
    let fallbackNextArg = MZ.guard (paramsPosition > lastEndPosition) $> lastIndex
    let foundIndex = NonEmptyArray.findIndex (isInRange paramsPosition) absoluteRanges
    foundIndex <|> fallbackNextArg # orElse "Couldn't find index. "

getExpression ∷ String -> Either String (NonEmptyArray Range)
getExpression text = case parseExpr text of
  ParseSucceeded succ -> case succ of
    CST.ExprApp _ args -> Right (sourceRangeToRange <$> rangeOf <$> args)
    CST.ExprIdent other -> Left ("Ident " <> stringify (unsafeCoerce other))
    other -> Left ("Nope " <> stringify (unsafeCoerce other))
  ParseSucceededWithErrors (CST.ExprApp _ args) _ -> Right (sourceRangeToRange <$> rangeOf <$> args)
  ParseSucceededWithErrors _ _ -> Left ("Parsed as something else than an expr app")
  ParseFailed { error } -> case error of
    -- ExpectedToken (CST.TokChar _ '"') _ -> getExpression (text <> "\"")
    -- ExpectedToken (CST.TokChar _ '(') _ -> getExpression (text <> "dummy)")
    UnexpectedToken CST.TokRightParen
      | text # endsWith "()" -> getExpression (CodeUnits.dropRight 1 text <> "dummy)")
    -- Opening a parenthesis:
    UnexpectedEof
      | text # endsWith "(" -> getExpression (text <> "dummy)")
    LexExpected "quote" "end of file" -> getExpression (text <> "\"")
    _ -> Left ("Could really not parse " <> printParseError error)

-- { error :: ParseError
-- , position :: SourcePos
-- , tokens :: Array SourceToken
-- }
sourceRangeToRange ∷ CST.SourceRange -> Range
sourceRangeToRange sr =
  Range
    { start: sourcePosToPosition sr.start
    , end: sourcePosToPosition sr.end
    }

-- SourcePos are 0 based 
sourcePosToPosition ∷ CST.SourcePos -> Position
sourcePosToPosition { line, column } = Position { line: line + 1, character: column + 1 }

-- 
toAbsoluteRange ∷ Position -> Range -> Range
toAbsoluteRange reference (Range { start, end }) =
  Range
    { start: toAbsolutePosition reference start
    , end: toAbsolutePosition reference end
    }

toAbsolutePosition ∷ Position -> Position -> Position
toAbsolutePosition (Position reference) (Position relative) =
  Position
    { line: reference.line + (relative.line - 1) -- 1 based
    , character:
      if relative.line == 1 then
        reference.character + (relative.character - 1)
      else
        relative.character
    }

isInRange ∷ Position -> Range -> Boolean
isInRange position (Range { start, end }) = position >= start && position <= end

startSignatureHelp ∷ Ref (Maybe State)  -> DocumentStore -> Int -> 
  { identToModule :: Object Module
  , identifiers :: Array String
  , main :: Maybe String
  , modules :: Array Module
  } -> SignatureHelpParams -> ExceptT String Aff SignatureHelp
startSignatureHelp stateRef docs port modules params@{ textDocument } = do
  maybeDoc <- getDocument docs (uri textDocument) # liftEffect
  case maybeDoc of
    Nothing -> throwError "document not found"
    Just doc -> do
      text <- getTextAtRange doc (lineRange startPosition) # liftEffect
      { word, qualifier } <- identifierAtPoint text (startPosition ^. character) # orElse "no identifier at point"
      let unqualifiedModules = getUnqualActiveModules modules (Just word)
      let qualifiedModules state = getQualModule state modules
      ideTypeInfo <- getTypeInfo port word modules.main qualifier unqualifiedModules qualifiedModules <#> note "no type info" # ExceptT
      typeInfo@{ signatureHelp } <- typeInfoToSignatureHelp word ideTypeInfo # orElse "unparseable type"
      -- The position on which the function starts
      let wordStartPosition = startPosition # character %~ (_ - String.length word)
      storeData word wordStartPosition typeInfo # liftEffect
      pure signatureHelp
  where
  startPosition ∷ Position
  -- Move one character to the left because a space (" ") triggers the completion
  startPosition = params.position # character %~ (_ - 1)

  storeData ∷ String -> Position -> _ -> Effect Unit
  storeData functionName functionStartPosition typeInfo = Ref.write (Just toWrite) stateRef
    where
    toWrite =
      Zipper.fromTree
        $ mkTree
            { startPosition: functionStartPosition
            , maxPosition: params.position
            , parameters: typeInfo.parameters
            , functionName
            , previousResponse: typeInfo.signatureHelp
            }
            Nil

typeInfoToSignatureHelp ∷ String -> C.TypeInfo -> Maybe { parameters ∷ NonEmptyArray String, signature ∷ CST.Type Void, signatureHelp ∷ SignatureHelp }
typeInfoToSignatureHelp word (C.TypeInfo { type', expandedType, documentation }) = do
  case parseSignature (fromMaybe type' expandedType) of
    Nothing -> Nothing
    Just { signature, parameters } -> do
      let
        label = word <> " " <> NonEmptyArray.intercalate " " parameters
        parameterOffsets = parameters # parametersToOffsets (String.length word)
        parameterInformation ∷ Array ParameterInformation
        parameterInformation = parameterOffsets <#> cast <<< { label: _ }
        signatureHelp =
          SignatureHelp
            { activeParameter: Nullable.notNull 0
            , activeSignature: Nullable.notNull 0
            , signatures:
              NonEmptyArray.singleton
                ( cast
                    { label
                    , documentation: markupContent $ typeStr <> "\n" <> (fromMaybe "" documentation)
                    , parameters: parameterInformation
                    }
                )
            }
      Just { signatureHelp, signature, parameters }
  where
  typeStr = "```purescript\n" <> compactTypeStr -- <> (if showExpanded then "\n" <> expandedTypeStr else "") <> "\n```"


  compactTypeStr = word <> " :: " <> type'

  expandedTypeStr = word <> " :: " <> (fromMaybe "" expandedType)

-- | Turns an array of string types into their offsets required by the LSP
-- | to highlight the currently active argument in Signature help
parametersToOffsets ∷ Int -> NonEmptyArray String -> Array Offsets
parametersToOffsets initialOffset =
  _.offsets
    <<< foldl fn { offsets: [], current: initialOffset }
    <<< NonEmptyArray.toArray
  where
  fn { offsets, current } currentArg =
    { offsets: Array.snoc offsets (mkOffsets start end)
    , current: end
    }
    where
    start = current + 1

    end = start + String.length currentArg

printBack ∷ ∀ tof. TokensOf tof => tof -> Array String
printBack t = [ fold (printSourceToken <$> (TokenList.toArray (tokensOf t))) ]

parseIt ∷ CST.Type Void -> Array String
parseIt = case _ of
  -- A function falls under this category. Interesting!
  CST.TypeKinded (CST.TypeVar functionName) sourceToken signature -> do
    -- let _ = spy "type kinded named" { signature: printBack signature }
    parseIt signature
  CST.TypeKinded other sourceToken nestedType2 -> do
    -- let _ = spy "type kinded other" { other: printBack other, nestedType2: printBack nestedType2 }
    parseIt other <> parseIt nestedType2
  CST.TypeForall _ _ _ t -> do
    -- let _ = spy "type forall" { t: printBack t }
    parseIt t
  CST.TypeArrow current _ next -> do
    -- let _ = spy "type arr" {}
    [ String.trim $ fold (printSourceToken <$> (TokenList.toArray (tokensOf current))) ] <> parseIt next
  CST.TypeApp nestedType neaArgs -> do
    -- let _ = spy "type app" {}
    parseIt nestedType --["type app"]
  CST.TypeVar (CST.Name { token, name: CST.Ident name }) -> do
    -- let _ = spy "type var" {}
    []
  -- We don't care for these because they need nothing applied to them   
  CST.TypeConstructor (CST.QualifiedName qn@{ token, name }) -> []
  CST.TypeWildcard sourceToken -> [ "something else" ]
  CST.TypeHole (CST.Name { token, name }) -> [ "something else" ]
  CST.TypeString sourceToken s -> [ "type string" ]
  CST.TypeRow (CST.Wrapped { open, value, close }) -> [ "something else" ]
  CST.TypeRecord (CST.Wrapped { open, value, close }) -> [ "something else" ]
  CST.TypeOp nestedType neaTupleQualifiedNameAndType -> [ "type op" ]
  CST.TypeOpName (CST.QualifiedName qn@{ token, name }) -> [ "op name" ]
  -- CST.TypeArr nestedType1 sourceToken nestedType2 -> ["type arr"]
  CST.TypeArrowName sourceToken -> [ "arr name" ]
  CST.TypeConstrained constraint _ nested -> parseIt nested
  CST.TypeParens (CST.Wrapped nestedType) -> [ "parens" ]
  CST.TypeUnaryRow sourceToken nestedType -> [ "unary row" ]
  CST.TypeError e -> [ "type error" ]

parseSignature ∷ String -> Maybe { signature ∷ CST.Type Void, parameters ∷ NonEmptyArray String }
parseSignature s = do
  case parseType s of
    ParseSucceeded signature -> do
      case NonEmptyArray.fromArray (parseIt signature) of
        Just parameters -> Just { signature, parameters }
        Nothing -> Nothing
    _ -> Nothing

typeToParameters ∷ String -> Maybe Unit
typeToParameters typeString = case parseType typeString of
  ParseSucceeded t -> case t of
    -- Interesting one:
    CST.TypeApp nestedType neaArgs -> Just unit
    CST.TypeVar (CST.Name { token, name }) -> Nothing
    CST.TypeConstructor (CST.QualifiedName qn@{ token, name }) -> Nothing
    CST.TypeWildcard sourceToken -> Nothing
    CST.TypeHole (CST.Name { token, name }) -> Nothing
    CST.TypeString sourceToken s -> Nothing
    CST.TypeRow (CST.Wrapped { open, value, close }) -> Nothing
    CST.TypeRecord (CST.Wrapped { open, value, close }) -> Nothing
    CST.TypeForall st neaTypeVarBinding sourceToken nestedType -> Nothing
    CST.TypeKinded nestedType1 sourceToken nestedType2 -> Nothing
    CST.TypeOp nestedType neaTupleQualifiedNameAndType -> Nothing
    CST.TypeOpName (CST.QualifiedName qn@{ token, name }) -> Nothing
    CST.TypeArrow nestedType1 sourceToken nestedType2 -> Nothing
    CST.TypeArrowName sourceToken -> Nothing
    CST.TypeConstrained nestedType1 sourceToken nestedType2 -> Nothing
    CST.TypeParens (CST.Wrapped nestedType) -> Nothing
    CST.TypeUnaryRow sourceToken nestedType -> Nothing
    CST.TypeError e -> Nothing
  _ -> Nothing

wordRange ∷ ∀ a. Position -> { left ∷ Int, right ∷ Int | a } -> Range
wordRange (Position { line }) { left, right } =
  Range
    { start: Position { line, character: left }
    , end: Position { line, character: right }
    }

lineRange ∷ Position -> Range
lineRange (Position { line, character }) =
  Range
    { start:
      Position
        { line
        , character: 0
        }
    , end:
      Position
        { line
        , character: character + 100
        }
    }
