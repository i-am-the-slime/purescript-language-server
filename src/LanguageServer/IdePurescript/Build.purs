module LanguageServer.IdePurescript.Build where

import Prelude

import Data.Array (filter, mapMaybe, notElem, uncons)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import IdePurescript.Build (Command(Command), build, rebuild, rebuildWithTmpFile)
import IdePurescript.PscErrors (PscResult(..))
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Config (addNpmPath, buildCommand, censorCodes, codegenTargets)
import LanguageServer.IdePurescript.Server (loadAll)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Types (Diagnostic(Diagnostic), DocumentStore, DocumentUri, Position(Position), Range(Range), Settings)
import LanguageServer.Uri (uriToFilename)
import Node.Path (resolve)
import PscIde.Command (RebuildError(RebuildError))
import PscIde.Command as PC

positionToRange :: PC.RangePosition -> Range
positionToRange ({ startLine, startColumn, endLine, endColumn}) =
  Range { start: Position { line: startLine-1, character: startColumn-1 }
        , end:   Position { line: endLine-1, character: endColumn-1 } }

type DiagnosticResult = { pscErrors :: Array RebuildError, hasErrors :: Boolean, diagnostics :: Object (Array Diagnostic) }

emptyDiagnostics :: DiagnosticResult
emptyDiagnostics = { pscErrors: [], hasErrors: false, diagnostics: Object.empty }

collectByFirst :: forall a. Array (Tuple (Maybe String) a) -> Object (Array a)
collectByFirst x = Object.fromFoldableWith (<>) $ mapMaybe f x
  where
  f (Tuple (Just a) b) = Just (Tuple a [b])
  f _ = Nothing

convertDiagnostics :: String -> Settings -> PscResult -> Effect DiagnosticResult
convertDiagnostics projectRoot settings (PscResult { warnings, errors }) =
    diagnostics <#>
      { diagnostics: _
      , pscErrors: errors <> warnings'
      , hasErrors: not (Array.null errors)
      }
  where
  diagnostics :: Effect (Object (Array Diagnostic))
  diagnostics = do
    diags <- allDiagnostics
    pure $ collectByFirst diags

  allDiagnostics :: Effect (Array (Tuple (Maybe String) Diagnostic))
  allDiagnostics =
      traverse (convertDiagnostic true) errors <>
      traverse (convertDiagnostic false) warnings'

  warnings' = censorWarnings settings warnings
  dummyRange = 
      Range { start: Position { line: 1, character: 1 }
            , end:   Position { line: 1, character: 1 } }

  convertDiagnostic :: Boolean -> RebuildError -> Effect (Tuple (Maybe String) Diagnostic)
  convertDiagnostic isError (RebuildError { errorCode, position, message, filename }) = do
    resolvedFile <- traverse (resolve [ projectRoot ]) filename
    pure $ Tuple resolvedFile 
      (Diagnostic
        { range: maybe dummyRange positionToRange position
        , severity: toNullable $ Just $ if isError then 1 else 2 
        , code: toNullable $ Just $ errorCode
        , source: toNullable $ Just "PureScript"
        , message
        })

getDiagnostics :: DocumentUri -> Settings -> ServerState -> Aff DiagnosticResult
getDiagnostics uri settings state = do 
  filename <- liftEffect $ uriToFilename uri
  let targets = codegenTargets settings
  case state of
    ServerState { port: Just port, root: Just root } -> do
      { errors, success } <- rebuild port filename targets
      liftEffect $ convertDiagnostics root settings errors
    _ -> pure emptyDiagnostics

getDiagnosticsForTmpFile :: String -> DocumentUri -> Settings -> ServerState -> Aff DiagnosticResult
getDiagnosticsForTmpFile tmpFilename uri settings state = do 
  filename <- liftEffect $ uriToFilename uri
  let targets = codegenTargets settings
  case state of
    ServerState { port: Just port, root: Just root } -> do
      { errors, success } <- rebuildWithTmpFile port tmpFilename filename targets
      liftEffect $ convertDiagnostics root settings errors
    _ -> pure emptyDiagnostics

censorWarnings :: Settings -> Array RebuildError -> Array RebuildError
censorWarnings settings = filter (flip notElem codes <<< getCode)
  where
    getCode (RebuildError { errorCode }) = errorCode
    codes = censorCodes settings

foreign import parseShellQuote :: String -> Array String

fullBuild :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff (Either String DiagnosticResult)
fullBuild notify _ settings state _ = do
  let command = parseShellQuote $ buildCommand settings
  case state, uncons command of
    ServerState { port: maybePort, root: Just directory }, Just { head: cmd, tail: args } -> do
      build notify { command: Command cmd args, directory, useNpmDir: addNpmPath settings }
        >>= either (pure <<< Left) \{errors} -> do
          liftEffect $ notify Info "Build complete"
          case maybePort of 
            Nothing -> liftEffect $ notify Error $ "Couldn't reload modules, no ide server port"
            Just port -> do
              attempt (loadAll port) >>= case _ of
                Left e -> liftEffect $ notify Error $ "Error reloading modules: " <> show e
                Right (Left msg) -> liftEffect $ notify Error $ "Error message from IDE server reloading modules: " <> msg
                _ -> liftEffect $ notify Info "Reloaded modules"
          liftEffect $ Right <$> convertDiagnostics directory settings errors
    _, Nothing ->
      pure $ Left "Error parsing build command"
    ServerState { port, root }, _ -> do
      pure $ Left $ "Error running build: port=" <> show port <> ", root=" <> show root
