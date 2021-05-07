module IdePurescript.Typeclass where

import Prelude

import Data.Foldable (for_)
import Data.Nullable (null)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeToForeign)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Types (DocumentStore, Settings)
import LanguageServer.Protocol.Window (showInformation)

handleAddInstance ∷ DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
handleAddInstance _ _ (ServerState { connection }) _ = do
  for_ connection \c -> showInformation c "Hi!" # liftEffect
  pure $ unsafeToForeign null