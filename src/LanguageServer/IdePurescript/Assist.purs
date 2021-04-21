module LanguageServer.IdePurescript.Assist where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (fold, intercalate, take, (!!))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over)
import Data.String (trim)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (F, Foreign, readInt, readString, unsafeFromForeign, unsafeToForeign)
import Foreign.Index ((!))
import Foreign.NullOrUndefined (readNullOrUndefined)
import IdePurescript.Completion (declarationTypeToNamespace)
import IdePurescript.PscIde (eitherToErr)
import IdePurescript.PscIdeServer (Notify)
import IdePurescript.Tokens (containsArrow, identifierAtPoint, startsWithCapitalLetter)
import LanguageServer.Console (log)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (applyEdit)
import LanguageServer.IdePurescript.Commands as Commands
import LanguageServer.IdePurescript.Imports (addCompletionImport, addCompletionImport', addCompletionImportEdit, showNS)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Text (makeWorkspaceEdit)
import LanguageServer.TextDocument (getText, getTextAtRange, getVersion)
import LanguageServer.Types (Command, DocumentStore, DocumentUri(..), Position(..), Range(..), Settings, readRange)
import PscIde (defaultCompletionOptions, suggestTypos)
import PscIde as P
import PscIde.Command (DeclarationType(..), TypeInfo(..), declarationTypeFromString, declarationTypeToString)

lineRange' :: Int -> Int -> Range
lineRange' line character = lineRange $ Position { line, character }

lineRange :: Position -> Range
lineRange pos@(Position { line, character }) = Range 
    { start: pos # over Position (_ { character = 0 })
    , end: pos # over Position (_ { character = (top :: Int) })
    }

caseSplit :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
caseSplit docs settings state args = do
  let ServerState { port, connection, clientCapabilities } = state
  case port, connection, args of
    Just port', Just connection', [ argUri, argLine, argChar, argType ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        , Right tyStr <- runExcept $ readString argType
        -> do
            doc <- liftEffect $ getDocument docs (DocumentUri uri)
            lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
            version <- liftEffect $ getVersion doc
            case identifierAtPoint lineText char of
                Just { range: { left, right } } -> do
                    lines <- eitherToErr $ P.caseSplit port' lineText left right false tyStr
                    let edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version (lineRange' line char) $ intercalate "\n" $ map trim lines
                    void $ applyEdit connection' edit
                _ -> do liftEffect $ log connection' "fail identifier"
                        pure unit
            pure unit
    _, Just conn', [ argUri, argLine, argChar, argType ] ->
        liftEffect $ log conn' $ show [ show $ runExcept $ readString argUri, show $ runExcept $ readInt argLine , show $ runExcept $ readInt argChar, show $ runExcept $ readString argType ]
    _, _, _ -> do 
        liftEffect $ maybe (pure unit) (flip log "fail match") connection
        pure unit

addClause :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
addClause docs settings state args = do
  let ServerState { port, connection, clientCapabilities } = state
  case port, connection, args of
    Just port', Just connection', [ argUri, argLine, argChar ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        -> do
            doc <- liftEffect $ getDocument docs (DocumentUri uri)
            lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)

            version <- liftEffect $ getVersion doc
            case identifierAtPoint lineText char of
                Just { range: { left, right } } -> do
                    lines <- eitherToErr $ P.addClause port' lineText false
                    let edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version (lineRange' line char) $ intercalate "\n" $ map trim lines
                    void $ applyEdit connection' edit
                _ -> pure unit
            pure unit
    _, _, _ -> pure unit

newtype TypoResult 
  = TypoResult 
      { identifier :: String
      , mod :: String
      , declarationType :: String 
      }

encodeTypoResult :: TypoResult -> Foreign
encodeTypoResult = unsafeToForeign

decodeTypoResult :: Foreign -> F TypoResult
decodeTypoResult obj = do
  identifier <- obj ! "identifier" >>= readString
  mod <- obj ! "mod" >>= readString
  declarationType <- fromMaybe "" <$> 
    (obj ! "declarationType" >>= readNullOrUndefined readString)
  pure $ TypoResult { identifier, mod , declarationType }

fixTypoActions :: DocumentStore -> Settings -> ServerState -> DocumentUri -> Int -> Int -> Aff (Array Command)
fixTypoActions docs settings state@(ServerState { port, connection, modules, clientCapabilities }) docUri line char =
  case port, connection of
    Just port', Just connection' -> do
      doc <- liftEffect $ getDocument docs docUri
      lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
      version <- liftEffect $ getVersion doc
      case identifierAtPoint lineText char of
        Just { word, range } -> do
          res <- suggestTypos port' word 2 modules.main defaultCompletionOptions
          pure $ case res of 
            Left _ -> []
            Right infos ->
              take 10 $
              map (\tinfo@(TypeInfo { type', identifier, module', declarationType }) ->
                Commands.fixTypo' 
                  do
                    let decTypeString = renderDeclarationType type' identifier declarationType
                    if identifier == word then
                      "Import" <> decTypeString <> identifier <> " (" <> module' <> ")"
                    else
                      "Replace with " <> identifier <> " (" <> module' <> ")"  
                  docUri line char
                  (encodeTypoResult $ TypoResult { identifier, mod: module', declarationType: maybe "" declarationTypeToString declarationType }))
                infos
        Nothing -> pure []
    _, _ -> pure []
    where
      renderDeclarationType type' identifier = case _ of
        Nothing -> " : "
        Just DeclTypeOperator -> " type-level operator: "
        Just DeclType -> " type: "
        Just DeclTypeSynonym -> " type synonym: "
        Just DeclDataConstructor -> " data constructor: "
        Just DeclTypeClass -> " type class: "
        Just DeclValueOperator -> " operator: "
        Just DeclModule -> " module: " -- I don't really think this will happen
        Just DeclValue -> 
          if startsWithCapitalLetter identifier then
          " data constructor: " else 
          if containsArrow type' then
          " function: " else
          " value: "

fixTypo :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
fixTypo log docs settings state@(ServerState { port, connection, modules, clientCapabilities }) args = do
  unsafeToForeign <$> case port, connection, args !! 0, args !! 1, args !! 2 of
    Just port', Just conn', Just argUri, Just argLine, Just argChar
      | Right uri <- runExcept $ readString argUri
      , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
      , Right char <- runExcept $ readInt argChar -> do
        doc <- liftEffect $ getDocument docs (DocumentUri uri)
        lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
        version <- liftEffect $ getVersion doc
        case identifierAtPoint lineText char, (runExcept <<< decodeTypoResult) <$> args !! 3 of
            Just { range }, Just (Right (TypoResult { identifier, mod, declarationType })) -> 
              void $ replace conn' uri version line range identifier mod declarationType
            _, _ -> pure unit
    _, _, _, _, _ -> pure unit

  where
    replace conn' uri version line {left, right} word mod declarationType = do 
      let range = Range { start: Position { line, character: left }
                        , end: Position { line, character: right }
                        }
          edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version range word
      -- TODO suggestion type
          namespace = declarationTypeToNamespace =<< declarationTypeFromString declarationType
      addCompletionImport' edit log docs settings state [ unsafeToForeign word, unsafeToForeign mod, unsafeToForeign Nothing, unsafeToForeign uri, unsafeToForeign (maybe "" showNS namespace) ]

fillTypedHole :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
fillTypedHole logFn docs settings state args = do
  let ServerState { port, connection, clientCapabilities } = state
  case port, connection, args of
    Just port', Just conn', [ _, argUri, range', argChoice ]
      | Right range <- runExcept $ readRange range'
      , Right uri <- runExcept $ readString argUri
      , TypeInfo { identifier, module': mod } <- readTypeInfo argChoice
     -> do
      doc <- liftEffect $ getDocument docs (DocumentUri uri)
      version <- liftEffect $ getVersion doc
      text <- liftEffect $ getText doc
      let edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version range identifier
      edit' <- either (const []) identity <$> addCompletionImportEdit logFn docs settings state
         { identifier, mod: Just mod , qual: Nothing, uri: DocumentUri uri }
        doc version text Nothing
      let edit2 = edit <> fold edit'
      applyRes <- applyEdit conn' $ edit2 -- edit <> fold edit'
      liftEffect $ log conn' $ "Applied: " <> show applyRes
      -- -- Seems that even after waiting for the edit response, changes will be lost 
      -- delay $ Milliseconds 300.0
      _ <- addCompletionImport logFn docs settings state [ unsafeToForeign identifier, unsafeToForeign mod, unsafeToForeign Nothing, unsafeToForeign uri ]
      pure unit
    _, _, _ -> do 
      liftEffect $ maybe (pure unit) (flip log "fail match") connection
      pure unit
  where
    readTypeInfo :: Foreign -> TypeInfo
    readTypeInfo obj = unsafeFromForeign obj