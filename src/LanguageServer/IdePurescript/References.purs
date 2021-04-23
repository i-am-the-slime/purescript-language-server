module LanguageServer.IdePurescript.References where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, un)
import Data.String.Utils (endsWith)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getTypeInfo)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (ReferenceParams)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.Position (convertTypePosition)
import LanguageServer.Protocol.TextDocument (getTextAtRange)
import LanguageServer.Protocol.Types (DocumentStore, Location(..), Position(..), Range(..), Settings, TextDocumentIdentifier(..))
import LanguageServer.Protocol.Uri (filenameToUri)
import Node.Path (resolve)
import PscIde (usages)
import PscIde.Command (Namespace(..))
import PscIde.Command as Command

getReferences :: DocumentStore -> Settings -> ServerState -> ReferenceParams
  -> Aff (Array Location)
getReferences docs _ state ({ textDocument, position }) = do
    maybeDoc <- liftEffect $ getDocument docs (_.uri $ un TextDocumentIdentifier textDocument)
    case maybeDoc of
      Nothing -> pure []
      Just doc -> do
        text <- liftEffect $ getTextAtRange doc (mkRange position)
        let { pscIdePort, modules, root } = un ServerState $ state
        case pscIdePort, root, identifierAtPoint text (_.character $ un Position position) of
          Just port', Just root', Just { word, qualifier } -> do
            info <- getTypeInfo port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)
            case info of
              Just (Command.TypeInfo { module', type' }) -> do
                let ns = case type' of
                          "Type" -> NSType
                          _ | endsWith "-> Type" type' -> NSType
                          _ -> NSValue

                usg <- usages port' module' ns word
                
                liftEffect $ either (pure $ pure []) (traverse $ convLocation root') usg
              _ -> pure []
          _, _, _ -> pure []
    where


    convLocation :: String -> Command.TypePosition -> Effect Location
    convLocation root typePosition@(Command.TypePosition { name }) = do
      uri <- filenameToUri =<< resolve [ root ] name
      pure $ Location 
        { uri
        , range: convertTypePosition typePosition
        }

    mkRange pos = Range
        { start: pos # over Position (_ { character = 0 })
        , end: pos # over Position (\c -> c { character = c.character + 100 })
        }
    
