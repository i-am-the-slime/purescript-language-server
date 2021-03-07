module LanguageServer.IdePurescript.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Fiber, Milliseconds)
import Foreign (Foreign)
import Foreign.Object (Object)
import IdePurescript.Modules (State)
import LanguageServer.TextDocument (TextDocument)
import LanguageServer.Types (Connection, DocumentStore, DocumentUri, Settings, ClientCapabilities)
import PscIde.Command (RebuildError)

newtype ServerState = ServerState
  { port :: Maybe Int
  , deactivate :: Aff Unit
  , root :: Maybe String
  , connection :: Maybe Connection
  , runningRebuild :: Maybe (Fiber Unit)
  , buildQueue :: Object TextDocument
  , successfulBuildTimes :: Object (Array Milliseconds)
  , modules :: State
  , modulesFile :: Maybe DocumentUri
  , buildQueue :: Object TextDocument
  , diagnostics :: Object (Array RebuildError)
  , clientCapabilities :: Maybe ClientCapabilities
  }

derive instance newtypeServerState :: Newtype ServerState _

type CommandHandler a = DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff a
