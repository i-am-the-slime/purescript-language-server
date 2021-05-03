module LanguageServer.IdePurescript.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Fiber, Milliseconds)
import Foreign (Foreign)
import Foreign.Object (Object)
import IdePurescript.Modules as Modules
import LanguageServer.Protocol.TextDocument (TextDocument)
import LanguageServer.Protocol.Types (ClientCapabilities, Connection, DocumentStore, DocumentUri, Settings)
import PscIde.Command (RebuildError)
import PureScript.CST (RecoveredParserResult)
import PureScript.CST.Types as CST

newtype ServerState = ServerState
  { pscIdePort :: Maybe Int
  , deactivate :: Aff Unit
  , root :: Maybe String
  , connection :: Maybe Connection
  , runningRebuild :: Maybe (Fiber Unit)
  , previousRebuild :: Maybe { uri :: DocumentUri, content :: String }
  , successfulBuildTimes :: Object (Array Milliseconds)
  , showExportManagementCodeLenses :: Boolean
  , modules :: Modules.State
  , modulesFile :: Maybe DocumentUri
  , buildQueue :: Object TextDocument
  , diagnostics :: Object (Array RebuildError)
  , parseResults :: Object VersionedParseResult
  , clientCapabilities :: Maybe ClientCapabilities
  }

type VersionedParseResult = { version :: Number, result :: RecoveredParserResult CST.Module } 

derive instance newtypeServerState :: Newtype ServerState _

type CommandHandler a = DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff a
