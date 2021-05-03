module LanguageServer.IdePurescript.CodeLenses where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff (Aff, attempt, joinFiber, message)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import LanguageServer.IdePurescript.CodeLens.ExportManagement (exportManagementCodeLenses)
import LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations (topLevelDeclarationCodeLenses)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.Handlers (CodeLensParams, CodeLensResult)
import LanguageServer.Protocol.Types (DocumentStore, Settings, TextDocumentIdentifier(..))

getCodeLenses ∷ Ref ServerState -> DocumentStore -> Settings -> ServerState -> CodeLensParams -> Aff (Array CodeLensResult)
getCodeLenses stateRef documentStore _ _ { textDocument: TextDocumentIdentifier { uri } } = do
  ServerState { runningRebuild } <- Ref.read stateRef # liftEffect
  case runningRebuild of
    Just fib -> do
      result <- attempt (joinFiber fib)
      case result of
        Left e | message e == "Debounced build" -> pure []
        Left e -> do
          ServerState { connection } <- Ref.read stateRef # liftEffect
          for_ connection \c -> log c (message e) # liftEffect
          pure []
        Right _ -> run
    Nothing -> run
  where
  run = do
    ServerState { diagnostics, showExportManagementCodeLenses, parseResults } <- Ref.read stateRef # liftEffect
    topLevelDeclarations <- topLevelDeclarationCodeLenses diagnostics uri
    exportManagement  <- guard showExportManagementCodeLenses $ exportManagementCodeLenses parseResults documentStore uri
    pure $ topLevelDeclarations <> exportManagement 
