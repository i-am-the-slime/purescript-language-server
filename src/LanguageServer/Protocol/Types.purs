module LanguageServer.Protocol.Types where

import Prelude
import Data.Array (concat, groupBy, sortWith, (:))
import Data.Array.NonEmpty (toNonEmpty)
import Data.Either (Either, either)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over)
import Data.NonEmpty ((:|))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Semigroup.Foldable (foldl1)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, readInt)
import Foreign.Index ((!))
import Foreign.Object (Object)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)

foreign import data Connection ∷ Type

foreign import data DocumentStore ∷ Type

type MarkedString =
  { language ∷ String, value ∷ String }

markedString ∷ String -> MarkedString
markedString s = { language: "purescript", value: s }

type MarkupContent =
  { kind ∷ String, value ∷ String }

markupContent ∷ String -> MarkupContent
markupContent s = { kind: "markdown", value: s }

derive instance newtypeDocumentUri ∷ Newtype DocumentUri _

newtype DocumentUri = DocumentUri String

instance showDocumentUri ∷ Show DocumentUri where
  show (DocumentUri rawUri) = "DocumentUri " <> show rawUri

derive newtype instance eqDocumentUri ∷ Eq DocumentUri

newtype Position = Position { line ∷ Int, character ∷ Int }

character ∷ Lens' Position Int
character = _Newtype <<< prop (SProxy ∷ _ "character")

line ∷ Lens' Position Int
line = _Newtype <<< prop (SProxy ∷ _ "line")

derive instance eqPosition ∷ Eq Position

instance positionOrd ∷ Ord Position where
  compare (Position p1) (Position p2)
    | p1.line < p2.line = LT
    | p1.line == p2.line && p1.character < p2.character = LT
    | p1.line == p2.line && p1.character == p2.character = EQ
    | otherwise = GT

derive instance newtypePosition ∷ Newtype Position _

instance showPosition ∷ Show Position where
  show (Position p) = "Position(" <> show p.line <> "," <> show p.character <> ")"

newtype Range = Range { start ∷ Position, end ∷ Position }

instance eqRange ∷ Eq Range where
  eq (Range { start, end }) (Range { start: start', end: end' }) = start == start' && end == end'

instance ordRange ∷ Ord Range where
  compare (Range { start, end }) (Range { start: start', end: end' }) = compare start start' <> compare end end'

instance showRange ∷ Show Range where
  show (Range { start, end }) = "Range(" <> show start <> "," <> show end <> ")"

endPosition ∷ Lens' Range Position
endPosition = _Newtype <<< prop (SProxy ∷ _ "end")

startPosition ∷ Lens' Range Position
startPosition = _Newtype <<< prop (SProxy ∷ _ "start")

derive instance newtypeRange ∷ Newtype Range _

readRange ∷ Foreign -> F Range
readRange r = do
  start <- r ! "start" >>= readPosition
  end <- r ! "end" >>= readPosition
  pure $ Range { start, end }
  where
  readPosition p = ado
    l <- p ! "line" >>= readInt
    c <- p ! "character" >>= readInt
    in Position { line: l, character: c }

newtype Location = Location { uri ∷ DocumentUri, range ∷ Range }

derive instance newtypeLocation ∷ Newtype Location _

newtype Diagnostic = Diagnostic
  { range ∷ Range
  , severity ∷ Nullable Int -- 1 (Error) - 4 (Hint)
  , code ∷ Nullable String -- String | Int
  , source ∷ Nullable String
  , message ∷ String
  }

derive instance newtypeDiagnostic ∷ Newtype Diagnostic _
derive newtype instance showDiagnostic ∷ Show Diagnostic

newtype CompletionItem = CompletionItem
  { label ∷ String
  , kind ∷ Nullable Int
  , detail ∷ Nullable String
  , documentation ∷ Nullable MarkupContent
  , sortText ∷ Nullable String
  , filterText ∷ Nullable String
  , insertText ∷ Nullable String
  , textEdit ∷ Nullable TextEdit
  , additionalTextEdits ∷ Nullable (Array TextEdit)
  , command ∷ Nullable Command
  }

derive instance newtypeCompletionItem ∷ Newtype CompletionItem _

data CompletionItemKind
  = Text
  | Method
  | Function
  | Constructor
  | Field
  | Variable
  | Class
  | Interface
  | Module
  | Property
  | Unit
  | Value
  | Enum
  | Keyword
  | Snippet
  | Color
  | File
  | Reference

defaultCompletionItem ∷ String -> CompletionItem
defaultCompletionItem label =
  CompletionItem
    { label
    , kind: toNullable Nothing
    , detail: toNullable Nothing
    , documentation: toNullable Nothing
    , sortText: toNullable Nothing
    , filterText: toNullable Nothing
    , insertText: toNullable $ Just label
    , textEdit: toNullable Nothing
    , additionalTextEdits: toNullable Nothing
    , command: toNullable Nothing
    }

completionItem ∷ String -> CompletionItemKind -> CompletionItem
completionItem label k = defaultCompletionItem label # over CompletionItem (_ { kind = toNullable $ Just $ completionItemKindToInt k })

completionItemKindToInt ∷ CompletionItemKind -> Int
completionItemKindToInt = case _ of
  Text -> 1
  Method -> 2
  Function -> 3
  Constructor -> 4
  Field -> 5
  Variable -> 6
  Class -> 7
  Interface -> 8
  Module -> 9
  Property -> 10
  Unit -> 11
  Value -> 12
  Enum -> 13
  Keyword -> 14
  Snippet -> 15
  Color -> 16
  File -> 17
  Reference -> 18

newtype CompletionItemList = CompletionItemList
  { isIncomplete ∷ Boolean
  , items ∷ Array CompletionItem
  }

derive instance newtypeCompletionList ∷ Newtype CompletionItemList _

newtype SymbolInformation = SymbolInformation
  { name ∷ String
  , kind ∷ Int
  , location ∷ Location
  , containerName ∷ Nullable String
  }

data SymbolKind
  = FileSymbolKind
  | ModuleSymbolKind
  | NamespaceSymbolKind
  | PackageSymbolKind
  | ClassSymbolKind
  | MethodSymbolKind
  | PropertySymbolKind
  | FieldSymbolKind
  | ConstructorSymbolKind
  | EnumSymbolKind
  | InterfaceSymbolKind
  | FunctionSymbolKind
  | VariableSymbolKind
  | ConstantSymbolKind
  | StringSymbolKind
  | NumberSymbolKind
  | BooleanSymbolKind
  | ArraySymbolKind

symbolKindToInt ∷ SymbolKind -> Int
symbolKindToInt = case _ of
  FileSymbolKind -> 1
  ModuleSymbolKind -> 2
  NamespaceSymbolKind -> 3
  PackageSymbolKind -> 4
  ClassSymbolKind -> 5
  MethodSymbolKind -> 6
  PropertySymbolKind -> 7
  FieldSymbolKind -> 8
  ConstructorSymbolKind -> 9
  EnumSymbolKind -> 10
  InterfaceSymbolKind -> 11
  FunctionSymbolKind -> 12
  VariableSymbolKind -> 13
  ConstantSymbolKind -> 14
  StringSymbolKind -> 15
  NumberSymbolKind -> 16
  BooleanSymbolKind -> 17
  ArraySymbolKind -> 18

newtype Hover = Hover { contents ∷ MarkupContent, range ∷ Nullable Range }

newtype Command = Command { title ∷ String, command ∷ String, arguments ∷ Nullable (Array Foreign) }

derive instance newtypeCommand ∷ Newtype Command _

newtype CodeAction = CodeAction
  { title ∷ String
  , kind ∷ CodeActionKind
  , isPreferred ∷ Boolean
  , edit ∷ Nullable WorkspaceEdit
  , command ∷ Nullable Command
  }

foreign import data CodeActionResult ∷ Type

codeActionResult ∷ Either CodeAction Command -> CodeActionResult
codeActionResult = either unsafeCoerce unsafeCoerce

newtype TextEdit = TextEdit { range ∷ Range, newText ∷ String }

derive instance newtypeTextEdit ∷ Newtype TextEdit _

instance eqTextEdit ∷ Eq TextEdit where
  eq (TextEdit { range, newText }) (TextEdit { range: range', newText: newText' }) = range == range' && newText == newText'

instance showTextEdit ∷ Show TextEdit where
  show (TextEdit { range, newText }) = ("TextEdit(" <> show range <> ", " <> show newText <> ")")

newtype WorkspaceEdit = WorkspaceEdit
  { documentChanges ∷ Nullable (Array TextDocumentEdit)
  , changes ∷ Nullable (Object (Array TextEdit))
  }

instance semigroupWorkspaceEdit ∷ Semigroup WorkspaceEdit where
  append (WorkspaceEdit { documentChanges, changes }) (WorkspaceEdit { documentChanges: documentChanges', changes: changes' }) =
    WorkspaceEdit
      { documentChanges:
        toNullable
          $ case toMaybe documentChanges, toMaybe documentChanges' of
              Nothing, Nothing -> Nothing
              _, _ ->
                Just
                  $ map (foldl1 combine)
                  $ map toNonEmpty
                  $ groupBy (\d1 d2 -> docId d1 == docId d2)
                      (fromNullableArray documentChanges <> fromNullableArray documentChanges')
      , changes:
        toNullable
          $ case toMaybe changes, toMaybe changes' of
              Nothing, Nothing -> Nothing
              _, _ -> Just $ Object.fromFoldableWith (<>) (goStrMap changes <> goStrMap changes')
      }
    where
    combine (TextDocumentEdit { textDocument, edits }) (TextDocumentEdit { edits: edits' }) = TextDocumentEdit { textDocument, edits: edits <> edits' }

    docId (TextDocumentEdit { textDocument }) = textDocument

    fromNullableArray ∷ ∀ a. Nullable (Array a) -> Array a
    fromNullableArray a = fromMaybe [] $ toMaybe a

    goStrMap ∷ ∀ a. Nullable (Object a) -> Array (Tuple String a)
    goStrMap a = Object.toUnfoldable $ fromMaybe Object.empty $ toMaybe a

instance monoidWorkspaceEdit ∷ Monoid WorkspaceEdit where
  mempty = WorkspaceEdit { documentChanges: toNullable Nothing, changes: toNullable Nothing }

-- | Create workspace edit, supporting both documentChanges and older changes property for v2 clients
workspaceEdit ∷ Maybe ClientCapabilities -> Array TextDocumentEdit -> WorkspaceEdit
workspaceEdit capabilities edits =
  WorkspaceEdit
    { documentChanges:
      toNullable
        $ if useDocumentChanges then Just edits else Nothing
    , changes:
      toNullable
        $ if useDocumentChanges then
            Nothing
          else
            Just
              $ Object.fromFoldable
              $ map (\(h :| t) -> Tuple (getUri h) (concat $ getEdit h : map getEdit t))
              $ map toNonEmpty
              $ groupBy (\a b -> getUri a == getUri b)
              $ sortWith getUri edits
    }
  where
  useDocumentChanges = supportsDocumentChanges capabilities

  getUri (TextDocumentEdit { textDocument: TextDocumentIdentifier { uri: DocumentUri docUri } }) = docUri

  getEdit (TextDocumentEdit tde) = tde.edits

supportsDocumentChanges ∷ Maybe ClientCapabilities -> Boolean
supportsDocumentChanges Nothing = false

supportsDocumentChanges (Just { workspace }) = fromMaybe false $ toMaybe workspace >>= (_.workspaceEdit >>> toMaybe) >>= (_.documentChanges >>> toMaybe)

newtype TextDocumentEdit = TextDocumentEdit { textDocument ∷ TextDocumentIdentifier, edits ∷ Array TextEdit }

newtype TextDocumentIdentifier = TextDocumentIdentifier { uri ∷ DocumentUri, version ∷ Number }

uri ∷ TextDocumentIdentifier -> DocumentUri
uri (TextDocumentIdentifier tdi) = tdi.uri

derive instance newtypeTextDocumentIdentifier ∷ Newtype TextDocumentIdentifier _
derive instance eqTextDocumentIdentifier ∷ Eq TextDocumentIdentifier

type Settings =
  Foreign

newtype FileChangeTypeCode = FileChangeTypeCode Int

data FileChangeType
  = CreatedChangeType
  | ChangedChangeType
  | DeletedChangeType

derive instance eqFileChangeType ∷ Eq FileChangeType

fileChangeTypeToInt ∷ FileChangeType -> Int
fileChangeTypeToInt = case _ of
  CreatedChangeType -> 1
  ChangedChangeType -> 2
  DeletedChangeType -> 3

fromFileChangeTypeCode ∷ FileChangeTypeCode -> Maybe FileChangeType
fromFileChangeTypeCode = case _ of
  FileChangeTypeCode 1 -> Just CreatedChangeType
  FileChangeTypeCode 2 -> Just ChangedChangeType
  FileChangeTypeCode 3 -> Just DeletedChangeType
  _ -> Nothing

newtype FileEvent = FileEvent { uri ∷ DocumentUri, type ∷ FileChangeTypeCode }

newtype FoldingRange = FoldingRange
  { startLine ∷ Int
  , startCharacter ∷ Nullable Int
  , endLine ∷ Int
  , endCharacter ∷ Nullable Int
  , kind ∷ Nullable String -- | comment, imports, region
  }

type ClientCapabilities =
  { workspace ∷ Nullable WorkspaceClientCapabilities, textDocument ∷ Nullable TextDocumentClientCapabilities }

type WorkspaceClientCapabilities =
  { applyEdit ∷ Nullable Boolean, workspaceEdit ∷ Nullable WorkspaceEditClientCapabilities }

type TextDocumentClientCapabilities =
  { codeAction ∷ Nullable CodeActionClientCapabilities, codeLens ∷ Nullable CodeLensClientCapabilities }

type CodeActionClientCapabilities =
  { codeActionLiteralSupport ∷ Nullable { codeActionKind ∷ { valueSet ∷ Array CodeActionKind } }, isPreferredSupport ∷ Nullable Boolean }

type CodeLensClientCapabilities =
  { dynamicRegistration ∷ Nullable Boolean }

newtype CodeActionKind = CodeActionKind String

instance showCodeActionKind ∷ Show CodeActionKind where
  show (CodeActionKind s) = "CodeActionKind " <> s

codeActionEmpty ∷ CodeActionKind
codeActionEmpty = CodeActionKind ""

codeActionQuickFix ∷ CodeActionKind
codeActionQuickFix = CodeActionKind "quickfix"

codeActionRefactor ∷ CodeActionKind
codeActionRefactor = CodeActionKind "refactor"

codeActionRefactorExtract ∷ CodeActionKind
codeActionRefactorExtract = CodeActionKind "refactor.extract"

codeActionRefactorInline ∷ CodeActionKind
codeActionRefactorInline = CodeActionKind "refactor.inline"

codeActionRefactorRewrite ∷ CodeActionKind
codeActionRefactorRewrite = CodeActionKind "refactor.rewrite"

codeActionSource ∷ CodeActionKind
codeActionSource = CodeActionKind "source"

codeActionSourceOrganizeImports ∷ CodeActionKind
codeActionSourceOrganizeImports = CodeActionKind "source.organizeImports"

-- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspaceEditClientCapabilities
type WorkspaceEditClientCapabilities =
  { documentChanges ∷ Nullable Boolean }

newtype LanguageId = LanguageId String
