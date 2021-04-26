module LanguageServer.IdePurescript.ChangeContent where

import Prelude
import Control.Alt ((<|>))
import Data.Array (length)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (odd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over, un)
import Data.String.CodeUnits as String
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Time.Duration as Milliseconds
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now as Instant
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Internal.Stringify (unsafeStringify)
import Foreign.Object as Object
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.DocumentStore (TextDocumentChangeEvent)
import LanguageServer.Protocol.Handlers (publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.IdePurescript.Build (getDiagnosticsForTmpFile)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.TemporaryFile (withTemporaryFile)
import LanguageServer.Protocol.TextDocument (TextDocument, getText, getUri)
import LanguageServer.Protocol.Types (Connection, DocumentUri(..))
import LanguageServer.Protocol.Uri (uriToFilename)
import LanguageServer.Protocol.Window (showInformation, showWarning)

handleDidChangeContent ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Aff Unit ->
  (Aff Unit -> Effect (Fiber Unit)) ->
  TextDocumentChangeEvent ->
  Effect Unit
handleDidChangeContent configRef connection stateRef restartPscIdeServer launchAffLog { document } = do
  let uri = getUri document
  config <- Ref.read configRef # liftEffect
  state <- Ref.read stateRef # liftEffect
  filename <- uriToFilename uri # liftEffect
  let previousBuildTimes = fromMaybe [] (Object.lookup filename (un ServerState state).successfulBuildTimes)
  if Config.liveRebuild config && (not isTooSlow previousBuildTimes) then
    liveRebuild config connection stateRef uri restartPscIdeServer document filename previousBuildTimes launchAffLog
  else
    invalidateModulesFile stateRef

invalidateModulesFile ∷ Ref ServerState -> Effect Unit
invalidateModulesFile = Ref.modify_ (over ServerState (_ { modulesFile = Nothing }))

debounceMessage :: String
debounceMessage ="Debounced build"

liveRebuild ∷ Foreign -> Connection -> Ref ServerState -> DocumentUri -> Aff Unit -> TextDocument -> String -> Array Milliseconds -> (Aff Unit -> Effect (Fiber Unit)) -> Effect Unit
liveRebuild config connection stateRef uri restartPscIdeServer document realFilename previousBuildTimes launchAffLog = do
  state ∷ ServerState <- Ref.read stateRef # liftEffect
  currentlyRunning <- Ref.read stateRef <#> un ServerState <#> _.runningRebuild
  for_ currentlyRunning (Aff.launchAff_ <<< Aff.killFiber (Aff.error debounceMessage))
  runningRebuild <-
    launchAffLog
      $ compileFile
          config
          connection
          stateRef
          state
          document
          restartPscIdeServer
          uri
          realFilename
          previousBuildTimes
  Ref.modify_ (over ServerState (_ { runningRebuild = Just runningRebuild })) stateRef

compileFile ∷ Foreign -> Connection -> Ref ServerState -> ServerState -> TextDocument -> Aff Unit -> DocumentUri -> String -> Array Milliseconds -> Aff Unit
compileFile config conn stateRef state document restartPscIdeServer uri realFilename previousBuildTimes = do
  -- [TODO]: 
  -- Reorder the function definitions intelligently in such a way that wherever
  -- the latest change happened is the first function
  -- 
  Aff.delay (Config.rebuildFrequency config)
  textContent <- liftEffect $ getText document
  let
    textDidChange = case (un ServerState state).previousRebuild of
      Nothing -> true
      Just previous -> not (previous.uri == uri && previous.content == textContent)
  -- Nothing should change in this case
  when textDidChange do
    -- create a copy of the file so that we can check it with
    -- purs ide without having to save the current document
    withTemporaryFile realFilename textContent \tmpFilename -> do
      let rebuild = rebuildFile stateRef config state conn uri previousBuildTimes realFilename tmpFilename textContent
      let kill = killServer conn stateRef restartPscIdeServer state previousBuildTimes realFilename
      Aff.sequential (Aff.parallel rebuild <|> Aff.parallel kill)

rebuildFile ∷ Ref ServerState -> Foreign -> ServerState -> Connection -> DocumentUri -> Array Milliseconds -> String -> String -> String -> Aff Unit
rebuildFile stateRef config state conn uri previousBuildTimes realFilename tmpFilename textContent = do
  Milliseconds startedAt <- Instant.now <#> unInstant # liftEffect
  { pscErrors, diagnostics, hasErrors } <- getDiagnosticsForTmpFile tmpFilename uri config state
  let fileDiagnostics = fromMaybe [] $ Object.lookup realFilename diagnostics
  liftEffect do
    sendDiagnosticsBegin conn # liftEffect
    Milliseconds finishedAt <- Instant.now <#> unInstant # liftEffect
    let buildTime = Milliseconds $ finishedAt - startedAt
    -- showWarning conn ("Rebuild took " <> show (un Milliseconds buildTime) <> "ms " <> String.takeRight 20 (unsafeStringify uri))
    let
      newSuccessfulBuildTimes =
        -- errors usually get reported quickly so we exclude them 
        if hasErrors then
          previousBuildTimes
        else
          Array.takeEnd 5 (Array.snoc previousBuildTimes buildTime)
    when (isTooSlow newSuccessfulBuildTimes) do
      showWarning conn
        ( "This file builds very slowly.\n"
            <> "I will not build it as you type anymore.\n"
            <> "You must save this file to get error information."
        )
      # liftEffect
    -- Once we're done
    --  1. Start publishing diagnostics
    --  2. Remove the fiber
    --  3. Record successful build time
    --  4. Persist diagnostics
    --  4. Persist previousRebuild
    --  5. Finish sending diagnostics
    log conn $ "Built with " <> show (length fileDiagnostics) <> "/" <> show (length pscErrors) <> " issues for file: " <> show realFilename <> ", all diagnostic files: " <> show (Object.keys diagnostics)
    let nonFileDiagnostics = Object.delete realFilename diagnostics
    when (Object.size nonFileDiagnostics > 0) do
      log conn $ "Unmatched diagnostics: " <> show nonFileDiagnostics
    Ref.modify_
      ( over ServerState
          ( \s1 ->
              s1
                { runningRebuild = Nothing
                , successfulBuildTimes = Object.insert realFilename newSuccessfulBuildTimes (un ServerState state).successfulBuildTimes
                , diagnostics = Object.insert (un DocumentUri uri) pscErrors s1.diagnostics
                , previousRebuild = Just { content: textContent, uri }
                }
          )
      )
      stateRef
    publishDiagnostics conn { uri, diagnostics: fileDiagnostics }
    sendDiagnosticsEnd conn

killServer ∷ Connection -> Ref ServerState -> Aff Unit -> ServerState -> Array Milliseconds -> String -> Aff Unit
killServer conn stateRef restartPscIdeServer state previousBuildTimes realFilename = do
  delay (Seconds 8.0 # Milliseconds.fromDuration)
  let
    newSuccesfulBuildTimes =
      Array.takeEnd 3
        ( previousBuildTimes
            <> [ Seconds 8.0 # Milliseconds.fromDuration ]
        )
  showWarning conn "Extremely long compile time, restarting the PureScript language server" # liftEffect
  restartPscIdeServer
  when (isTooSlow newSuccesfulBuildTimes)
    $ liftEffect do
        showInformation conn "This file builds very slowly and I won't build it as you type anymore"
        showWarning conn "You must save this file to get error information"
  -- Once we're done
  --  1. Remove the fiber
  --  2. Record UNsuccessful build time 
  let
    update =
      over ServerState
        ( _
            { runningRebuild = Nothing
            , successfulBuildTimes = Object.insert realFilename newSuccesfulBuildTimes (un ServerState state).successfulBuildTimes
            }
        )
  Ref.modify_ update stateRef # liftEffect

isTooSlow ∷ Array Milliseconds -> Boolean
isTooSlow times = median times > (4.0 # Seconds # fromDuration)

median ∷ Array Milliseconds -> Milliseconds
median arr =
  fromMaybe mempty do
    let sorted = Array.sort arr
    if odd (Array.length arr) then
      sorted Array.!! (Array.length arr / 2)
    else do
      Milliseconds hi <- sorted Array.!! (Array.length arr / 2)
      Milliseconds lo <- sorted Array.!! ((Array.length arr / 2) - 1)
      pure (Milliseconds ((hi + lo) / 2.0))
