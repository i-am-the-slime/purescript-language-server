module LanguageServer.IdePurescript.FoldingRanges where

import Prelude

import Data.Array (findIndex, findLastIndex, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (FoldingRangesParams)
import LanguageServer.IdePurescript.Types (ServerState)
import LanguageServer.Protocol.TextDocument (getText)
import LanguageServer.Protocol.Types (DocumentStore, FoldingRange(..), Settings, TextDocumentIdentifier(..))

getFoldingRanges :: DocumentStore -> Settings -> ServerState -> FoldingRangesParams -> Aff (Array FoldingRange)
getFoldingRanges docs _ _ { textDocument: TextDocumentIdentifier textDocId } =
  liftEffect do
    maybeDoc <- getDocument docs textDocId.uri
    case maybeDoc of
      Just doc -> do
        text <- getText doc
        pure <<< fromFoldable
          $ getImportsSection text
      Nothing -> mempty

newlineRegex :: Regex
newlineRegex = unsafeRegex "[\n\r]" noFlags

getImportsSection :: String -> Maybe FoldingRange
getImportsSection text = ado
  firstImport <- findIndex isImportLine lines
  lastImport <- findLastIndex isImportLine lines
  in FoldingRange
    { startLine: firstImport
    , endLine: lastImport
    , startCharacter: Nullable.notNull 0
    , endCharacter:
      Nullable.notNull
        $ CodeUnits.length importPrefix - 1
    , kind: Nullable.notNull "imports"
    }
  where
  importPrefix = "import "
  lines = Regex.split newlineRegex text
  isImportLine = startsWith importPrefix
