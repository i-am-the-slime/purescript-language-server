module LanguageServer.Handlers where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import LanguageServer.Types (CodeActionResult, CompletionItemList, Connection, Diagnostic, DocumentUri, FileEvent, FoldingRange, Hover, Location, Position, Range, SymbolInformation, TextDocumentIdentifier, TextEdit, WorkspaceEdit, MarkupContent)
import Literals.Undefined (Undefined)
import Type.Row (type (+))
import Untagged.Union (type (|+|), UndefinedOr)

type TextDocumentPositionParamsRow r =
  ( textDocument ∷ TextDocumentIdentifier, position ∷ Position | r )

type TextDocumentPositionParams =
  { | TextDocumentPositionParamsRow () }

type DocumentSymbolParams =
  { textDocument ∷ TextDocumentIdentifier }

type ReferenceParams =
  TextDocumentPositionParams

type WorkspaceSymbolParams =
  { query ∷ String }

type CodeActionParams =
  { textDocument ∷ TextDocumentIdentifier, range ∷ Range, context ∷ CodeActionContext }

type CodeActionContext =
  { diagnostics ∷ Array Diagnostic }

type FoldingRangesParams =
  { textDocument ∷ TextDocumentIdentifier }

newtype SignatureHelpTriggerKind = SignatureHelpTriggerKind Int
instance showSignatureHelpTriggerKind :: Show SignatureHelpTriggerKind where
  show = case _ of
    x | x == signatureHelpInvoked -> "Invoked"
    x | x == signatureHelpTriggerCharacter -> "Trigger Character"
    x | x == signatureHelpContentChange -> "Content Change"
    _ -> "[ERROR]"

signatureHelpInvoked :: SignatureHelpTriggerKind
signatureHelpInvoked = SignatureHelpTriggerKind 1

signatureHelpTriggerCharacter :: SignatureHelpTriggerKind
signatureHelpTriggerCharacter = SignatureHelpTriggerKind 2

signatureHelpContentChange :: SignatureHelpTriggerKind
signatureHelpContentChange = SignatureHelpTriggerKind 3

derive newtype instance eqSignatureHelpTriggerKind ∷ Eq SignatureHelpTriggerKind

-- [TODO] Don't export constructor
newtype Offsets = Offsets (NonEmptyArray Int)

mkOffsets :: Int -> Int -> Offsets
mkOffsets inclusiveStart exclusiveEnd = 
  Offsets $ cons' inclusiveStart [ exclusiveEnd ]

type ParameterInformation =
  { label ∷ String |+| Offsets
  , documentation ∷ Undefined |+| String |+| MarkupContent
  }

-- [TODO] Don't export constructor
newtype MarkupKind = MarkupKind String

markupKindPlaintext :: MarkupKind
markupKindPlaintext = MarkupKind "plaintext"

markupKindMarkdown :: MarkupKind
markupKindMarkdown = MarkupKind "markdown"

type SignatureInformation =
  { label ∷ String
  , documentation ∷ Undefined |+| String |+| MarkupContent
  , parameters ∷ UndefinedOr (Array ParameterInformation)
  , activeParameter ∷ UndefinedOr Int
  }

newtype SignatureHelp = SignatureHelp
  { signatures ∷ NonEmptyArray SignatureInformation
  , activeSignature ∷ Nullable Int
  , activeParameter ∷ Nullable Int
  }

derive instance newtypeSignatureHelp :: Newtype SignatureHelp _

type SignatureHelpContext =
  { triggerKind ∷ SignatureHelpTriggerKind
  , triggerCharacter ∷ UndefinedOr String
  , isRetrigger ∷ Boolean
  , activeSignatureHelp ∷ UndefinedOr Boolean
  }

type SignatureHelpParamsRow r =
  ( context ∷ UndefinedOr SignatureHelpContext | r )

type SignatureHelpParams =
  { | TextDocumentPositionParamsRow + SignatureHelpParamsRow () }

type DocumentFormattingParams =
  { textDocument ∷ TextDocumentIdentifier }

type DidChangeConfigurationParams =
  { settings ∷ Foreign }

type PublishDiagnosticParams =
  { uri ∷ DocumentUri, diagnostics ∷ Array Diagnostic }

type ExecuteCommandParams =
  { command ∷ String, arguments ∷ Array Foreign }

type DidChangeWatchedFilesParams =
  { changes ∷ Array FileEvent }

type Result a =
  Effect (Promise a)

foreign import onDefinition ∷ Connection -> (TextDocumentPositionParams -> Result (Nullable Location)) -> Effect Unit

foreign import onCompletion ∷ Connection -> (TextDocumentPositionParams -> Result CompletionItemList) -> Effect Unit

foreign import onHover ∷ Connection -> (TextDocumentPositionParams -> Result (Nullable Hover)) -> Effect Unit

foreign import onSignatureHelp ∷ Connection -> (SignatureHelpParams -> Result (Nullable SignatureHelp)) -> Effect Unit

foreign import onDocumentSymbol ∷ Connection -> (DocumentSymbolParams -> Result (Array SymbolInformation)) -> Effect Unit

foreign import onWorkspaceSymbol ∷ Connection -> (WorkspaceSymbolParams -> Result (Array SymbolInformation)) -> Effect Unit

foreign import onReferences ∷ Connection -> (ReferenceParams -> Result (Array Location)) -> Effect Unit

foreign import onCodeAction ∷ Connection -> (CodeActionParams -> Result (Array CodeActionResult)) -> Effect Unit

foreign import onFoldingRanges ∷ Connection -> (FoldingRangesParams -> Result (Array FoldingRange)) -> Effect Unit

foreign import onDocumentFormatting ∷ Connection -> (DocumentFormattingParams -> Result (Array TextEdit)) -> Effect Unit

foreign import onDidChangeConfiguration ∷ Connection -> (DidChangeConfigurationParams -> Effect Unit) -> Effect Unit

foreign import onDidChangeWatchedFiles ∷ Connection -> (DidChangeWatchedFilesParams -> Effect Unit) -> Effect Unit

foreign import onExecuteCommand ∷ Connection -> (ExecuteCommandParams -> Effect (Promise Foreign)) -> Effect Unit

foreign import publishDiagnostics ∷ Connection -> PublishDiagnosticParams -> Effect Unit

foreign import applyEditImpl ∷ Connection -> WorkspaceEdit -> Result Boolean

applyEdit ∷ Connection -> WorkspaceEdit -> Aff Boolean
applyEdit conn edit = Promise.toAffE $ applyEditImpl conn edit

foreign import sendDiagnosticsBegin ∷ Connection -> Effect Unit

foreign import sendDiagnosticsEnd ∷ Connection -> Effect Unit

foreign import onExit ∷ Connection -> (Effect Unit) -> Effect Unit

foreign import onShutdown ∷ Connection -> (Result Unit) -> Effect Unit
