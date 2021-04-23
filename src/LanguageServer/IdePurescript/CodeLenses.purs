module LanguageServer.IdePurescript.CodeLenses where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt, joinFiber, message)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.Handlers (CodeLensParams, CodeLensResult)
import LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations (topLevelDeclarationCodeLenses)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Types (DocumentStore, Settings, TextDocumentIdentifier(..))

getCodeLenses ∷ Ref ServerState -> DocumentStore -> Settings -> ServerState -> CodeLensParams -> Aff (Array CodeLensResult)
getCodeLenses stateRef _ _ _ { textDocument: TextDocumentIdentifier { uri } } = do
  ServerState { runningRebuild } <- Ref.read stateRef # liftEffect
  case runningRebuild of
    Just fib -> do
      result <- attempt (joinFiber fib)
      case result of
        Left e
          | message e == "Debounced build" -> pure []
        Left e -> do
          ServerState { connection } <- Ref.read stateRef # liftEffect
          for_ connection \c -> log c (message e) # liftEffect
          pure []
        Right _ -> doIt
    Nothing -> doIt
  where
  doIt = do
    ServerState { diagnostics } <- Ref.read stateRef # liftEffect
    topLevelDeclarations <- topLevelDeclarationCodeLenses diagnostics uri
    -- liftEffect $ for_ connection \c -> showError c (unsafeStringify lenses)
    pure topLevelDeclarations
