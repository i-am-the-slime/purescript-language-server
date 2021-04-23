module LanguageServer.IdePurescript.Search where
  
import Prelude

import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError, readString, unsafeToForeign)
import Foreign.Index ((!))
import IdePurescript.Modules (getQualModule)
import IdePurescript.PscIde (getCompletion', getLoadedModules)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Types (DocumentStore, Settings)
import PscIde as P
import PscIde.Command (TypeInfo(..))
import PscIde.Command as C

newtype SearchResult = SearchResult { identifier :: String, typ :: String, mod :: String }

encodeSearchResult :: SearchResult -> Foreign
encodeSearchResult = unsafeToForeign

decodeSearchResult :: Foreign -> Except (NonEmptyList ForeignError) SearchResult
decodeSearchResult obj = do
  identifier <- obj ! "identifier" >>= readString
  typ <- obj ! "typ"  >>= readString
  mod <- obj ! "mod" >>= readString
  pure $ SearchResult { identifier, typ, mod }

search :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
search docs config state args = case state, runExcept $ traverse readString args of 
  ServerState { pscIdePort: Just pscIdePort, modules }, Right [ text ] -> do
    loadedModules <- getLoadedModules pscIdePort
    let getQualifiedModule = (flip getQualModule) modules
    results <- getCompletion' (Just $ C.Flex text) [] pscIdePort modules.main Nothing loadedModules getQualifiedModule P.defaultCompletionOptions
    pure $ unsafeToForeign $ toResult <$> results
  _, _ -> pure $ unsafeToForeign []

  where
  toResult (TypeInfo { type', identifier, module' }) = encodeSearchResult $ SearchResult { typ: type', identifier, mod: module' }
