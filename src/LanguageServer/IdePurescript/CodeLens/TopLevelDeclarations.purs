module LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations where

import Prelude
import Data.Array (mapMaybe)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable as Nullable
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import LanguageServer.Handlers (CodeLensResult)
import LanguageServer.IdePurescript.Commands (cmdName, replaceSuggestionCmd)
import LanguageServer.Types (Command(..), DocumentUri(..), Position(..), Range(..))
import PscIde.Command (PscSuggestion(..), RebuildError(..))

topLevelDeclarationCodeLenses ∷
  Object (Array RebuildError) -> DocumentUri -> Aff (Array CodeLensResult)
topLevelDeclarationCodeLenses diagnostics uri = do
  let fileDiagnostics = Object.lookup (un DocumentUri uri) diagnostics # fold
  pure $ codeLenses fileDiagnostics uri

codeLenses ∷
  Array RebuildError ->
  DocumentUri ->
  Array CodeLensResult
codeLenses diagnostics docUri =
  diagnostics
    # mapMaybe case _ of
        RebuildError
          { suggestion: Just (PscSuggestion { replacement: signature, replaceRange: Just range })
        , errorCode: "MissingTypeDeclaration"
        } ->
            Just
              { range: Range { start: Position { line: range.startLine-1, character: range.startColumn }, end: Position { line: range.endLine-1, character: range.endColumn } }
              , command:
                Nullable.notNull
                  $ Command
                      { command: cmdName replaceSuggestionCmd
                      , title: signature
                      , arguments:
                        Nullable.notNull
                          [ unsafeToForeign docUri
                          , unsafeToForeign signature
                          , unsafeToForeign range
                          ]
                      }
              , data: (Nullable.null # unsafeToForeign)
              }
        _ -> Nothing
