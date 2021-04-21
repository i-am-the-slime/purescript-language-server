module LanguageServer.IdePurescript.Assist where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (any, fold, foldl, intercalate, snoc, take, (!!))
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
lineRange pos = Range 
    { start: pos # over Position (_ { character = 0 })
    , end: pos # over Position (_ { character = (top :: Int) })
    }

caseSplit :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
caseSplit docs _ state args = do
  let ServerState { port, connection, clientCapabilities } = state
  case port, connection, args of
    Just port', Just connection', [ argUri, argLine, argChar, argType ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        , Right tyStr <- runExcept $ readString argType
        -> do
            maybeDoc <- liftEffect $ getDocument docs (DocumentUri uri)
            case maybeDoc of
              Nothing -> mempty
              Just doc -> do
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
addClause docs _ state args = do
  let ServerState { port, connection, clientCapabilities } = state
  case port, connection, args of
    Just port', Just connection', [ argUri, argLine, argChar ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        -> do
            maybeDoc <- liftEffect $ getDocument docs (DocumentUri uri)
            case maybeDoc of
              Nothing -> mempty
              Just doc -> do
                lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)

                version <- liftEffect $ getVersion doc
                case identifierAtPoint lineText char of
                    Just _ -> do
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
fixTypoActions docs _ (ServerState { port, modules }) docUri line char =
  case port of
    Just port' -> do
      maybeDoc <- liftEffect $ getDocument docs docUri
      case maybeDoc of 
        Nothing -> mempty
        Just doc -> do
          lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
          case identifierAtPoint lineText char of
            Nothing -> pure []
            Just { word } -> do
              res <- suggestTypos port' word 2 modules.main defaultCompletionOptions
              pure $ case res of 
                Left _ -> []
                Right infos ->
                  infos 
                    # simplifyImportChoice
                    <#> (\(TypeInfo { type', identifier, module', declarationType }) ->
                    Commands.fixTypo' 
                      do
                        let decTypeString = renderDeclarationType type' identifier declarationType
                        if identifier == word then
                          "Import" <> decTypeString <> identifier <> " (" <> module' <> ")"
                        else
                          "Replace with " <> identifier <> " (" <> module' <> ")"  
                      docUri line char
                      (encodeTypoResult $ TypoResult { identifier, mod: module', declarationType: maybe "" declarationTypeToString declarationType }))
                  # take 10
    _ -> pure []
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

-- | Removes choices that are not worth the brain-cycles to make.
-- | 
-- | If there are two suggestions to 
-- |   1. import Something(..) and
-- |   2. import Something 
-- | We only import Something(..) because it can be automatically simplified
-- | with an optimise import action
simplifyImportChoice :: Array TypeInfo -> Array TypeInfo
simplifyImportChoice before = foldl go [] before
  where
  go acc info = 
    if isType info && any (isTheSameButDataConstructor info) before then
      acc
    else
      snoc acc info 

  isType = case _ of 
    TypeInfo {declarationType: Just DeclType} -> true
    _ -> false

  isDataConstructor = case _ of 
    TypeInfo {declarationType: Just DeclDataConstructor} -> true
    TypeInfo {declarationType: Just DeclValue, identifier } | startsWithCapitalLetter identifier -> true
    _ -> false

  isTheSameButDataConstructor (TypeInfo ti1) info2@(TypeInfo ti2) =
    ti1.identifier == ti2.identifier &&
    ti1.module' == ti2.module' &&
    isDataConstructor info2

fixTypo :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
fixTypo log docs settings state@(ServerState { clientCapabilities }) args = do
  unsafeToForeign <$> case args !! 0, args !! 1, args !! 2 of
    Just argUri, Just argLine, Just argChar
      | Right uri <- runExcept $ readString argUri
      , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
      , Right char <- runExcept $ readInt argChar -> do
        maybeDoc <- liftEffect $ getDocument docs (DocumentUri uri)
        case maybeDoc of 
          Nothing -> mempty
          Just doc -> do
            lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
            version <- liftEffect $ getVersion doc
            case identifierAtPoint lineText char, (runExcept <<< decodeTypoResult) <$> args !! 3 of
                Just { range }, Just (Right (TypoResult { identifier, mod, declarationType })) -> 
                  void $ replace uri version line range identifier mod declarationType
                _, _ -> pure unit
    _, _, _ -> pure unit

  where
    replace uri version line {left, right} word mod declarationType = do 
      let range = Range { start: Position { line, character: left }
                        , end: Position { line, character: right }
                        }
          edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version range word
      -- TODO suggestion type
          namespace = declarationTypeToNamespace =<< declarationTypeFromString declarationType
      addCompletionImport' edit log docs settings state [ unsafeToForeign word, unsafeToForeign mod, unsafeToForeign Nothing, unsafeToForeign uri, unsafeToForeign (maybe "" showNS namespace) ]

fillTypedHole :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
fillTypedHole logFn docs settings state args = do
  let ServerState { connection, clientCapabilities } = state
  case connection, args of
    Just conn', [ _, argUri, range', argChoice ]
      | Right range <- runExcept $ readRange range'
      , Right uri <- runExcept $ readString argUri
      , TypeInfo { identifier, module': mod } <- readTypeInfo argChoice
     -> do
      maybeDoc <- liftEffect $ getDocument docs (DocumentUri uri)
      case maybeDoc of
        Nothing -> mempty
        Just doc -> do
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
    _, _ -> do 
      liftEffect $ maybe (pure unit) (flip log "fail match") connection
      pure unit
  where
    readTypeInfo :: Foreign -> TypeInfo
    readTypeInfo obj = unsafeFromForeign obj