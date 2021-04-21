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
import Foreign.Object as Object
import LanguageServer.Console (log)
import LanguageServer.DocumentStore (TextDocumentChangeEvent)
import LanguageServer.Handlers (publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.IdePurescript.Build (getDiagnosticsForTmpFile)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.TextDocument (getText, getUri)
import LanguageServer.Types (Connection, DocumentUri(..))
import LanguageServer.Uri (uriToFilename)
import LanguageServer.Window (showInformation, showWarning)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (unlink, writeTextFile)

handleDidChangeContent ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Aff Unit ->
  (Aff Unit -> Effect (Fiber Unit)) ->
  TextDocumentChangeEvent ->
  Effect Unit
handleDidChangeContent configRef conn stateRef restartPscIdeServer launchAffLog { document } = do
  let uri = getUri document
  config <- Ref.read configRef # liftEffect
  state ∷ ServerState <- Ref.read stateRef # liftEffect
  let invalidateModulesFile = Ref.modify_ (over ServerState (_ { modulesFile = Nothing })) stateRef
  realFilename <- liftEffect $ uriToFilename uri
  let previousBuildTimes = fromMaybe [] (Object.lookup realFilename (un ServerState state).successfulBuildTimes)
  let
    median arr =
      fromMaybe mempty do
        let sorted = Array.sort arr
        if odd (Array.length arr) then
          sorted Array.!! (Array.length arr / 2)
        else do
          Milliseconds hi <- sorted Array.!! (Array.length arr / 2)
          Milliseconds lo <- sorted Array.!! ((Array.length arr / 2) - 1)
          pure (Milliseconds ((hi + lo) / 2.0))
  let isTooSlow times = median times > (4.0 # Seconds # fromDuration)
  let
    compileDocument ∷ Effect Unit
    compileDocument =
      unless (isTooSlow previousBuildTimes) do
        let
          compileFile = do
            Aff.delay (Config.rebuildFrequency config)
            Milliseconds startedAt <- Instant.now <#> unInstant # liftEffect
            textContent <- liftEffect $ getText document
            liftEffect $ sendDiagnosticsBegin conn
            let filename = realFilename <> "$"
            -- create a copy of the file so that we can check it with
            -- purs ide without having to save the current document
            writeTextFile UTF8 filename textContent
            let deleteTemporaryFile = unlink filename
            let
              killServer = do
                delay (Seconds 8.0 # Milliseconds.fromDuration)
                let
                  newPreviousBuildTimes =
                    Array.takeEnd 3
                      ( previousBuildTimes
                          <> [ Seconds 8.0 # Milliseconds.fromDuration ]
                      )
                showWarning conn "Extremely long compile time, restarting the PureScript language server" # liftEffect
                restartPscIdeServer
                when (isTooSlow newPreviousBuildTimes)
                  $ liftEffect do
                      showInformation conn "This file builds very slowly and I won't build it as you type anymore"
                      showWarning conn "You must save this file to get error information"
                -- Once we're done
                --  1. Remove the fiber
                --  2. Record successful build time
                Ref.modify_
                  ( over ServerState
                      ( _
                          { runningRebuild = Nothing
                          , successfulBuildTimes = Object.insert realFilename newPreviousBuildTimes (un ServerState state).successfulBuildTimes
                          }
                      )
                  )
                  stateRef
                  # liftEffect
              rebuild = do
                { pscErrors, diagnostics, hasErrors } <- getDiagnosticsForTmpFile filename uri config state
                let fileDiagnostics = fromMaybe [] $ Object.lookup realFilename diagnostics
                liftEffect do
                  log conn $ "Built with " <> show (length fileDiagnostics) <> "/" <> show (length pscErrors) <> " issues for file: " <> show realFilename <> ", all diagnostic files: " <> show (Object.keys diagnostics)
                  let nonFileDiagnostics = Object.delete realFilename diagnostics
                  when (Object.size nonFileDiagnostics > 0) do
                    log conn $ "Unmatched diagnostics: " <> show nonFileDiagnostics
                  stateRef
                    # Ref.write
                        ( over ServerState
                            ( \s1 ->
                                s1
                                  { diagnostics = Object.insert (un DocumentUri uri) pscErrors (s1.diagnostics)
                                  }
                            )
                            state
                        )
                  publishDiagnostics conn { uri, diagnostics: fileDiagnostics }
                  sendDiagnosticsEnd conn
                  Milliseconds finishedAt <- Instant.now <#> unInstant # liftEffect
                  let buildTime = Milliseconds $ finishedAt - startedAt
                  showWarning conn ("Rebuild took " <> show buildTime)
                  let
                    newPreviousBuildTimes =
                      -- errors usually get reported quickly so we exclude them 
                      if hasErrors then
                        previousBuildTimes
                      else
                        Array.takeEnd 5 (Array.snoc previousBuildTimes buildTime)
                  when (isTooSlow newPreviousBuildTimes) do
                    showWarning conn
                      ( "This file builds very slowly.\n"
                          <> "I will not build it as you type anymore.\n"
                          <> "You must save this file to get error information."
                      )
                    # liftEffect
                  -- Once we're done
                  --  1. Remove the fiber
                  --  2. Record successful build time
                  Ref.modify_
                    ( over ServerState
                        ( _
                            { runningRebuild = Nothing
                            , successfulBuildTimes = Object.insert realFilename newPreviousBuildTimes (un ServerState state).successfulBuildTimes
                            }
                        )
                    )
                    stateRef
            Aff.finally deleteTemporaryFile
              $ Aff.sequential
                  (Aff.parallel rebuild <|> Aff.parallel killServer)
        currentlyRunning <- Ref.read stateRef <#> un ServerState <#> _.runningRebuild
        for_ currentlyRunning (Aff.launchAff_ <<< Aff.killFiber (Aff.error "Debounced build"))
        runningRebuild <- launchAffLog compileFile
        Ref.modify_ (over ServerState (_ { runningRebuild = Just runningRebuild })) stateRef
  if Config.liveRebuild config && (not isTooSlow previousBuildTimes) then
    compileDocument
  else
    invalidateModulesFile
