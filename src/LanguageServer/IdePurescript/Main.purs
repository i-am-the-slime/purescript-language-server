module LanguageServer.IdePurescript.Main (main) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Plus (empty)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (length, (\\))
import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..), either, hush)
import Data.Foldable (for_, or, fold)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, isNothing, maybe, maybe')
import Data.Newtype (over, un)
import Data.Nullable as Nullable
import Data.Profunctor.Strong (first)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), apathize, attempt, delay, forkAff, launchAff_, runAff, try)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Instant
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, unsafeToForeign)
import Foreign.JSON (parseJSON)
import Foreign.Object (Object)
import Foreign.Object as Object
import IdePurescript.Modules (getModulesForFileTemp, initialModulesState)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import IdePurescript.Spago as Spago
import LanguageServer.IdePurescript.Assist (addClause, caseSplit, fillTypedHole, fixTypo)
import LanguageServer.IdePurescript.Build (collectByFirst, fullBuild, getDiagnostics)
import LanguageServer.IdePurescript.ChangeContent (handleDidChangeContent)
import LanguageServer.IdePurescript.CodeActions (getActions, onReplaceAllSuggestions, onReplaceSuggestion)
import LanguageServer.IdePurescript.CodeLenses (getCodeLenses)
import LanguageServer.IdePurescript.Commands (addClauseCmd, addCompletionImportCmd, addModuleImportCmd, addSpagoDependencyCmd, buildCmd, caseSplitCmd, cmdName, commands, fixTypoCmd, getAvailableModulesCmd, listPackageSetPackagesCmd, organiseImportsCmd, replaceAllSuggestionsCmd, replaceSuggestionCmd, restartPscIdeCmd, searchCmd, startPscIdeCmd, stopPscIdeCmd, typedHoleExplicitCmd)
import LanguageServer.IdePurescript.Completion (getCompletions)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.FoldingRanges (getFoldingRanges)
import LanguageServer.IdePurescript.Formatting (getFormattedDocument)
import LanguageServer.IdePurescript.Imports (addCompletionImport, addModuleImport', getAllModules, organiseImports)
import LanguageServer.IdePurescript.References (getReferences)
import LanguageServer.IdePurescript.Search (search)
import LanguageServer.IdePurescript.Server as Server
import LanguageServer.IdePurescript.Symbols (getDefinition, getDocumentSymbols, getWorkspaceSymbols)
import LanguageServer.IdePurescript.Tooltips (getTooltips)
import LanguageServer.IdePurescript.Types (ServerState(..), CommandHandler)
import LanguageServer.IdePurescript.WatchedFiles (handleDidChangeWatchedFiles)
import LanguageServer.Protocol.Console (error, info, log, warn)
import LanguageServer.Protocol.DocumentStore (getDocument, onDidChangeContent, onDidOpenDocument, onDidSaveDocument)
import LanguageServer.Protocol.Handlers (onCodeAction, onCodeLens, onCompletion, onDefinition, onDidChangeConfiguration, onDidChangeWatchedFiles, onDocumentFormatting, onDocumentSymbol, onExecuteCommand, onFoldingRanges, onHover, onReferences, onShutdown, onWorkspaceSymbol, publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.Protocol.Setup (InitParams(..), getConfiguration, initConnection, initDocumentStore)
import LanguageServer.Protocol.TextDocument (TextDocument, getText, getUri)
import LanguageServer.Protocol.Types (Connection, Diagnostic, DocumentStore, DocumentUri(..), Settings, TextDocumentIdentifier(..))
import LanguageServer.Protocol.Uri (filenameToUri, uriToFilename)
import LanguageServer.Protocol.Window (showError, showWarningWithActions)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Node.FS.Sync as FSSync
import Node.Path (resolve)
import Node.Process (argv)
import Node.Process as Process
import PscIde.Command (RebuildError(..))

-- | main function parses the CLI arguments
-- | and calls handleConfig with parsed args to launch effects
main ∷ Effect Unit
main = do
  args <- parseArgs <$> argv
  for_ args.filename \filename ->
    FSSync.writeTextFile Encoding.UTF8 filename "Starting logging...\n"
  stateRef <- Ref.new defaultServerState
  configRef <- Ref.new (unsafeToForeign {})
  connection <- connect stateRef
  documents <- initDocumentStore connection
  let notify = mkNotify args.filename stateRef
  handleEvents configRef connection stateRef documents notify
  handleCommands configRef connection stateRef documents notify
  launchAff_ $ handleConfig configRef connection stateRef documents args.config notify
  log connection "PureScript Language Server started"

defaultServerState ∷ ServerState
defaultServerState =
  ServerState
    { pscIdePort: Nothing
    , deactivate: pure unit
    , root: Nothing
    , connection: Nothing
    , modules: initialModulesState
    , modulesFile: Nothing
    , buildQueue: Object.empty
    , runningRebuild: Nothing
    , previousRebuild: Nothing
    , successfulBuildTimes: Object.empty
    , diagnostics: Object.empty
    , clientCapabilities: Nothing
    }

type CmdLineArguments =
  { config ∷ Maybe Foreign
  , filename ∷ Maybe String
  }

defaultArguments ∷ CmdLineArguments
defaultArguments = { config: Nothing, filename: Nothing }

-- | Parses command line arguments  passed to process.argv
parseArgs ∷ Array String -> CmdLineArguments
parseArgs allArgs = parse defaultArguments relevantArgs
  where
  parse currentConfig = case _ of
    "--config" : conf : rest -> do
      let parsedConfig = hush $ runExcept $ parseJSON conf
      parse (currentConfig { config = parsedConfig }) rest
    "--log" : filename : rest -> parse (currentConfig { filename = Just filename }) rest
    _ : rest -> parse currentConfig rest
    List.Nil -> currentConfig

  relevantArgs ∷ List String
  relevantArgs = Array.toUnfoldable $ Array.drop 2 allArgs

updateModules ∷
  Ref ServerState -> DocumentStore -> DocumentUri -> Aff (Maybe ServerState)
updateModules stateRef documents uri = do
  currentState <- read stateRef
  case currentState of
    ServerState { pscIdePort: Just port, modulesFile }
      | modulesFile /= Just uri -> do
        maybeDoc <- getDocument documents uri # liftEffect
        case maybeDoc of 
          Nothing -> pure Nothing
          Just doc -> do
            text <- getText doc # liftEffect
            path <- uriToFilename uri # liftEffect
            modules <- getModulesForFileTemp port path text
            newState <-
              modify
                (over ServerState (_ { modules = modules, modulesFile = Just uri }))
                stateRef
            pure $ Just newState
    _ -> pure Nothing

mkRunHandler ∷
  Ref Foreign ->
  Ref ServerState ->
  DocumentStore ->
  String ->
  ∀ a b.
  (b -> Maybe DocumentUri) ->
  (Settings -> ServerState -> b -> Aff a) ->
  b ->
  Effect (Promise a)
mkRunHandler configRef stateRef documents _ getMaybeDocumentUri f b =
  Promise.fromAff do
    config <- read configRef
    let maybeDocumentUri = getMaybeDocumentUri b
    modules <- maybeDocumentUri # maybe empty (updateModules stateRef documents)
    state <- maybe' (\_ -> read stateRef) pure modules
    f config state b

-- | Extracts document uri value
getTextDocUri ∷
  ∀ r.
  { textDocument ∷ TextDocumentIdentifier | r } -> Maybe DocumentUri
getTextDocUri = (Just <<< _.uri <<< un TextDocumentIdentifier <<< _.textDocument)

mkNotify ∷ Maybe String -> Ref ServerState -> Notify
mkNotify logFile stateRef logLevel logMessage =
  liftEffect do
    ServerState { connection } <- Ref.read stateRef
    for_ connection logToConnection
    for_ logFile logToFile
  where
  logToConnection c =  logFunction c logMessage
    where
    logFunction = case logLevel of
      Success -> log
      Info -> info
      Warning -> warn
      Error -> error

  logToFile filename = do
    let taggedMessage = "[" <> show logLevel <> "] " <> logMessage <> "\n"
    FSSync.appendTextFile Encoding.UTF8 filename taggedMessage

-- | Stops IDE server
mkStopPscIdeServer ∷ Ref ServerState -> Notify -> Aff Unit
mkStopPscIdeServer stateRef notify = do
  ServerState { deactivate } <- read stateRef
  deactivate
  do
    modify_ (over ServerState $ _ { pscIdePort = Nothing, deactivate = pure unit }) stateRef
    notify Success "Stopped IDE server"

-- | Reads workspace root from state
getWorkspaceRoot ∷ ∀ eff. MonadEffect eff => Ref ServerState -> eff String
getWorkspaceRoot stateRef = do
    ServerState { root } <- read stateRef
    maybe (liftEffect Process.cwd) pure root

-- -- | Read connection from state
-- getConnection :: Ref ServerState -> Effect (Maybe Connection)
-- getConnection stateRef = do
--   (_.connection <<< unwrap) <$> Ref.read stateRef

-- | Read port from state
getPort ∷ ∀ eff. MonadEffect eff => Ref ServerState -> eff (Maybe Int)
getPort stateRef =
  liftEffect ado
    ServerState { pscIdePort } <- Ref.read stateRef
    in pscIdePort

-- | Builds documents in queue (which where opened on server startup)
buildDocumentsInQueue ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Effect Unit
buildDocumentsInQueue config connection stateRef = do
  ServerState { buildQueue } <- Ref.read stateRef
  let docs = Object.values buildQueue
  launchAff_
    $ for_ docs (rebuildAndSendDiagnostics config connection stateRef)

-- | Tries to start IDE server at workspace root
mkStartPscIdeServer ∷ Ref Foreign -> Connection -> Ref ServerState -> Notify -> Aff Unit
mkStartPscIdeServer config connection stateRef notify = do
  let workspaceRoot = getWorkspaceRoot stateRef
  liftEffect $ notify Info "Starting IDE server"
  rootPath <- liftEffect workspaceRoot
  settings <- liftEffect $ Ref.read config
  startRes <- Server.startServer' settings rootPath notify notify
  Server.retry notify 6 case startRes of
    { port: Just pscIdePort, quit } -> do
      Server.loadAll pscIdePort
        >>= case _ of
            Left msg ->
              liftEffect
                $ notify Info
                $ "Non-fatal error loading modules: "
                <> msg
            _ -> pure unit
      liftEffect
        $ Ref.modify_
            (over ServerState $ _ { pscIdePort = Just pscIdePort, deactivate = quit })
            stateRef
    _ -> pure unit
  liftEffect $ buildDocumentsInQueue config connection stateRef

connect ∷ Ref ServerState -> Effect Connection
connect stateRef =
  initConnection commands
    $ \({ params: InitParams { rootPath, rootUri, capabilities }, connection }) -> do
        Process.argv >>= \args -> log connection $ "Starting with args: " <> show args
        root <- case Nullable.toMaybe rootUri, Nullable.toMaybe rootPath of
          Just uri, _ -> Just <$> uriToFilename uri
          _, Just path -> pure $ Just path
          Nothing, Nothing -> pure Nothing
        workingRoot <- maybe Process.cwd pure root
        Ref.modify_
          ( over ServerState
              $ _
                  { root = Just workingRoot
                  , clientCapabilities = Just capabilities
                  }
          )
          stateRef
        ( \(Tuple dir root') ->
            log connection ("Starting with cwd: " <> dir <> " and using root path: " <> root')
        )
          =<< Tuple
          <$> Process.cwd
          <*> pure workingRoot
        Ref.modify_ (over ServerState $ _ { connection = Just connection }) stateRef

-- | Starts full build
buildProject ∷
  Connection ->
  Ref ServerState ->
  Notify ->
  DocumentStore -> Foreign -> ServerState -> Array Foreign -> Aff Unit
buildProject connection stateRef notify docs c s arguments = do
  let workspaceRoot = getWorkspaceRoot stateRef
  liftEffect $ sendDiagnosticsBegin connection
  fullBuild notify docs c s arguments
    >>= case _ of
        Right { pscErrors, diagnostics } ->
          liftEffect do
            log connection $ "Built with " <> (show $ length pscErrors) <> " issues"
            pscErrorsMap <-
              collectByFirst
                <$> traverse
                    ( \e@(RebuildError { filename }) -> do
                        projectRoot <- workspaceRoot
                        filename' <- traverse (resolve [ projectRoot ]) filename
                        uri <- maybe (pure Nothing) (\f -> Just <$> un DocumentUri <$> filenameToUri f) filename'
                        pure $ Tuple uri e
                    )
                    pscErrors
            prevErrors <- _.diagnostics <$> un ServerState <$> Ref.read stateRef
            let
              nonErrorFiles ∷ Array String
              nonErrorFiles = Object.keys prevErrors \\ Object.keys pscErrorsMap
            log connection $ "Removing old diagnostics for: " <> show nonErrorFiles
            for_ (map DocumentUri nonErrorFiles) \uri -> publishDiagnostics connection { uri, diagnostics: [] }
            write (over ServerState (_ { diagnostics = pscErrorsMap }) s) stateRef
            for_ (Object.toUnfoldable diagnostics ∷ Array (Tuple String (Array Diagnostic))) \(Tuple filename fileDiagnostics) -> do
              uri <- filenameToUri filename
              log connection $ "Publishing diagnostics for: " <> show uri <> " (" <> show filename <> ")"
              publishDiagnostics connection { uri, diagnostics: fileDiagnostics }
        Left err ->
          liftEffect do
            error connection err
            showError connection err
  liftEffect $ sendDiagnosticsEnd connection

mkLaunchAffLog ∷ ∀ a. Notify -> Aff a -> Effect (Fiber Unit)
mkLaunchAffLog notify = runAff $ 
  either (notify Error <<< show) 
    (const $ pure unit)

-- | Starts PscIDE Server if autoStart enabled in config.
autoStartPcsIdeServer ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  DocumentStore ->
  Aff Unit
autoStartPcsIdeServer configRef connection stateRef notify documents = do
  let workspaceRoot = getWorkspaceRoot stateRef
  let launchAffLog_ = void <$> mkLaunchAffLog notify
  let startPscIdeServer = mkStartPscIdeServer configRef connection stateRef notify
  let resolvePath p = workspaceRoot >>= \root -> resolve [ root ] p
  -- Ensure we only run once
  config <- liftEffect $ Ref.read configRef
  when (Config.autoStartPscIde config)
    $ do
        startPscIdeServer
        outputDir <-
          liftEffect
            $ resolvePath
            $ Config.effectiveOutputDirectory config
        hasPackageFile <-
          or
            <$> traverse (FS.exists <=< liftEffect <<< resolvePath)
                [ "bower.json", "psc-package.json", "spago.dhall" ]
        envIdeSources <- Server.getEnvPursIdeSources
        when (not hasPackageFile && isNothing envIdeSources) do
          liftEffect
            $ showError connection
                ( "It doesn't look like the workspace root is a PureScript project"
                    <> "(has bower.json/psc-package.json/spago.dhall)."
                    <> "The PureScript project should be opened as a root workspace folder."
                )
        exists <- FS.exists outputDir
        unless exists $ liftEffect
          $ launchAffLog_ do
              let message = "Output directory does not exist at '" <> outputDir <> "'"
              liftEffect $ info connection message
              let buildOption = "Build project"
              action <-
                showWarningWithActions connection
                  ( message
                      <> ". Ensure project is built, or check configuration of output directory"
                      <> " and build command."
                  )
                  [ buildOption ]
              when (action == Just buildOption) do
                s <- liftEffect $ Ref.read stateRef
                buildProject connection stateRef notify documents config s []

-- | Builds module and provides diagnostics
rebuildAndSendDiagnostics ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  TextDocument ->
  Aff Unit
rebuildAndSendDiagnostics configRef connection stateRef document = do
  let uri = getUri document
  config <- liftEffect $ Ref.read configRef
  state <- liftEffect $ Ref.read stateRef
  --organizeDiagnostics <- organiseImportsDiagnostic s notify document
  when (Config.fastRebuild config) do
    -- record start time
    startedAt <- Instant.now # liftEffect
    log connection "Starting fast rebuild" # liftEffect
    liftEffect $ sendDiagnosticsBegin connection
    { pscErrors, diagnostics, hasErrors } <- getDiagnostics uri config state
    filename <- liftEffect $ uriToFilename uri
    let fileDiagnostics = Object.lookup filename diagnostics # fold
    let previousBuildTimes = Object.lookup filename (un ServerState state).successfulBuildTimes # fold
    liftEffect do
      log connection
        $ "Built with "
        <> show (length fileDiagnostics)
        <> "/"
        <> show (length pscErrors)
        <> " issues for file: "
        <> show filename
        <> ", all diagnostic files: "
        <> show (Object.keys diagnostics)
      let nonFileDiagnostics = Object.delete filename diagnostics
      when (Object.size nonFileDiagnostics > 0) do
        log connection $ "Unmatched diagnostics: " <> show nonFileDiagnostics
      -- record end time
      finishedAt <- Instant.now # liftEffect
      -- take a window of a maximum of the previous five last build times
      let buildTime = timeDifference startedAt finishedAt
      let newPreviousBuildTimes = Array.takeEnd 5 (Array.snoc previousBuildTimes buildTime)
      stateRef # write
        (state # over ServerState
            ( \s1 ->
                s1
                  { diagnostics = Object.insert (un DocumentUri uri) pscErrors (s1.diagnostics)
                  , modulesFile = Nothing -- Force reload of modules on next request
                  , successfulBuildTimes =
                    (un ServerState state).successfulBuildTimes
                      # if hasErrors then identity else Object.insert filename newPreviousBuildTimes
                  }
            )
        )
      publishDiagnostics connection
        { uri
        , diagnostics: fileDiagnostics -- <> organizeDiagnostics
        }
      sendDiagnosticsEnd connection
      log connection 
        $ "Fast rebuild done in " <> show (un Milliseconds buildTime) <> "ms"

timeDifference :: Instant -> Instant -> Milliseconds
timeDifference start end = do
  let (Milliseconds msStart) = unInstant start
  let (Milliseconds msEnd) = unInstant end
  Milliseconds (msEnd - msStart)

-- | Registers event handlers
handleEvents ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  DocumentStore -> Notify -> Effect Unit
handleEvents configRef connection stateRef documents notify = do
  let
    runHandler = mkRunHandler configRef stateRef documents
    startPscIdeServer = mkStartPscIdeServer configRef connection stateRef notify
    stopPscIdeServer = mkStopPscIdeServer stateRef notify
    restartPscIdeServer = apathize stopPscIdeServer *> startPscIdeServer
    launchAffLog = mkLaunchAffLog notify
    launchAffLog_ = void <$> launchAffLog
  onCompletion connection
    $ runHandler
        "onCompletion"
        getTextDocUri
        (getCompletions documents)
  -- Handles go to definition
  onDefinition connection
    $ runHandler
        "onDefinition"
        getTextDocUri
        (getDefinition documents)
  onDocumentSymbol connection
    $ runHandler
        "onDocumentSymbol"
        getTextDocUri
        getDocumentSymbols
  onWorkspaceSymbol connection
    $ runHandler
        "onWorkspaceSymbol"
        (const Nothing)
        getWorkspaceSymbols
  onFoldingRanges connection
    $ runHandler
        "onFoldingRanges"
        getTextDocUri
        (getFoldingRanges documents)
  onDocumentFormatting connection
    $ runHandler
        "onDocumentFormatting"
        getTextDocUri
        (getFormattedDocument notify documents)
  onReferences connection
    $ runHandler
        "onReferences"
        getTextDocUri
        (getReferences documents)
  onHover connection
    $ runHandler
        "onHover"
        getTextDocUri
        (getTooltips documents)
  onCodeAction connection
    $ runHandler
        "onCodeAction"
        getTextDocUri
        (getActions documents)
  onCodeLens connection
    $ runHandler
        "onCodeLens"
        getTextDocUri
        (getCodeLenses stateRef documents)
  onShutdown connection $ Promise.fromAff stopPscIdeServer
  onDidChangeWatchedFiles connection
    $ launchAff_ 
    <<< handleDidChangeWatchedFiles configRef connection stateRef documents
  onDidChangeContent documents
    $ \textDocumentChangedEvent -> do
        handleDidChangeContent configRef connection stateRef restartPscIdeServer launchAffLog textDocumentChangedEvent
        Ref.modify_ (over ServerState (_ { modulesFile = Nothing })) stateRef
  -- On document opened rebuild it,
  -- or place it in a queue if no IDE server started
  onDidOpenDocument documents \{ document } ->
    launchAffLog_ do
      mbPort <- getPort stateRef
      if isJust mbPort then
        rebuildAndSendDiagnostics configRef connection stateRef document
      else do
        let uri = (un DocumentUri $ getUri document)
        liftEffect
          $ Ref.modify_
              ( over ServerState
                  ( \st ->
                      st
                        { buildQueue = Object.insert uri document st.buildQueue
                        }
                  )
              )
              stateRef
  onDidSaveDocument documents \{ document } ->
    launchAffLog_ do
      rebuildAndSendDiagnostics configRef connection stateRef document

handleConfig ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  DocumentStore ->
  Maybe Foreign -> Notify -> Aff Unit
handleConfig configRef connection stateRef documents cmdLineConfig notify = do
  let launchAffLog = void <$> mkLaunchAffLog notify
  gotConfig ∷ AVar Unit <- AVar.empty
  let
    setConfig ∷ String -> Foreign -> Aff Unit
    setConfig source newConfig = do
      liftEffect do
        log connection $ "Got new config (" <> source <> ")"
        Ref.write newConfig configRef
      AVar.tryPut unit gotConfig
        >>= if _ then pure unit
            else liftEffect $ notify Info "Not starting server, already started"
  liftEffect 
    $ onDidChangeConfiguration connection
    $ \{ settings } ->
        launchAffLog $ setConfig "client push" settings
  _ <-
    forkAff do
      -- Ensure we only run once
      AVar.read gotConfig
      autoStartPcsIdeServer configRef connection stateRef notify documents
  -- 1. Config on command line - go immediately
  for_ cmdLineConfig (setConfig "command line") 
  delay (50.0 # Milliseconds)
  -- 2. Config may be pushed immediately
  got1 <- AVar.isFilled <$> AVar.status gotConfig
  unless got1 do
    -- 3. Fetch config via pull 
    -- (waited 50ms as at least in vscode, not ready immediately)
    initialConfig <- attempt $ getConfiguration connection
    case initialConfig of
      Right ic -> setConfig "by request" ic
      Left error -> do
        liftEffect $ log connection 
          $ "Failed to request settings: " <> show error
        -- 4. Wait some time longer for possible config push, 
        -- then proceed with no config
        delay (200.0 # Milliseconds)
        got2 <- AVar.isFilled <$> AVar.status gotConfig
        unless got2 do
          liftEffect $ notify Warning "Proceeding with no config received"
          void $ AVar.tryPut unit gotConfig

-- | Registers commands handlers
handleCommands ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  DocumentStore ->
  Notify ->
  Effect Unit
handleCommands configRef connection stateRef documents notify = do
  let
    onBuild = buildProject connection stateRef notify
    stopPscIdeServer = mkStopPscIdeServer stateRef notify
    startPscIdeServer = mkStartPscIdeServer configRef connection stateRef notify
    restartPscIdeServer = do
      apathize stopPscIdeServer
      startPscIdeServer
    noResult = unsafeToForeign Nullable.null
    voidHandler ∷ ∀ a. CommandHandler a -> CommandHandler Foreign
    voidHandler handler docs config state arguments =
      try (handler docs config state arguments)
        >>= case _ of
            Left err -> do
              liftEffect $ notify Error $ show err
              pure noResult
            Right _ -> pure noResult
    simpleHandler handler _ _ _ _ = handler $> noResult
    handlers ∷ Object (CommandHandler Foreign)
    handlers =
      Object.fromFoldable $ first cmdName <$> 
        [ Tuple caseSplitCmd $ voidHandler caseSplit
        , Tuple addClauseCmd $ voidHandler addClause
        , Tuple replaceSuggestionCmd $ voidHandler onReplaceSuggestion
        , Tuple replaceAllSuggestionsCmd $ voidHandler onReplaceAllSuggestions
        , Tuple buildCmd $ voidHandler onBuild
        , Tuple addCompletionImportCmd $ addCompletionImport notify
        , Tuple addModuleImportCmd $ voidHandler $ addModuleImport' notify
        , Tuple organiseImportsCmd $ organiseImports notify
        , Tuple startPscIdeCmd $ simpleHandler startPscIdeServer
        , Tuple stopPscIdeCmd $ simpleHandler stopPscIdeServer
        , Tuple restartPscIdeCmd $ simpleHandler restartPscIdeServer
        , Tuple getAvailableModulesCmd $ getAllModules notify
        , Tuple searchCmd $ search
        , Tuple fixTypoCmd $ fixTypo notify
        , Tuple typedHoleExplicitCmd $ voidHandler $ fillTypedHole notify
        , Tuple addSpagoDependencyCmd $ Spago.addDependency
        , Tuple listPackageSetPackagesCmd $ Spago.getPackages
        ]
  onExecuteCommand connection
    $ \{ command, arguments } ->
        Promise.fromAff do
          config <- liftEffect $ Ref.read configRef
          state <- liftEffect $ Ref.read stateRef
          case Object.lookup command handlers of
            Just handler -> handler documents config state arguments
            Nothing -> do
              liftEffect $ error connection $ "Unknown command: " <> command
              pure noResult

-- Ref Helpers
modify ∷ ∀ eff a. MonadEffect eff => (a -> a) -> Ref a -> eff a
modify = (map >>> map) liftEffect Ref.modify

modify_ ∷ ∀ eff a. MonadEffect eff => (a -> a) -> Ref a -> eff Unit
modify_ = (map >>> map) liftEffect Ref.modify_

read ∷ ∀ eff a. MonadEffect eff => Ref a -> (eff a)
read = map liftEffect Ref.read

write :: forall eff a. MonadEffect eff => a -> Ref a -> eff Unit
write = (map >>> map) liftEffect Ref.write