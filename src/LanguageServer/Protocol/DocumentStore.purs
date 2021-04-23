module LanguageServer.Protocol.DocumentStore where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import LanguageServer.Protocol.TextDocument (TextDocument)
import LanguageServer.Protocol.Types (DocumentStore, DocumentUri)

foreign import getDocuments :: DocumentStore ->  Effect (Array TextDocument)

foreign import getDocumentImpl :: EffectFn4 (TextDocument -> Maybe TextDocument) (Maybe TextDocument) DocumentStore DocumentUri (Maybe TextDocument)

getDocument :: DocumentStore -> DocumentUri -> Effect (Maybe TextDocument)
getDocument = runEffectFn4 getDocumentImpl Just Nothing 

type TextDocumentChangeEvent = { document :: TextDocument }

foreign import onDidSaveDocument :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
foreign import onDidOpenDocument :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
foreign import onDidCloseDocument :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
foreign import onDidChangeContent :: DocumentStore -> (TextDocumentChangeEvent -> Effect Unit) -> Effect Unit
