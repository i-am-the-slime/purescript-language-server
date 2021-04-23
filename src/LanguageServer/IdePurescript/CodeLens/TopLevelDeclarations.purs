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
import LanguageServer.Protocol.Handlers (CodeLensResult)
import LanguageServer.IdePurescript.Commands (cmdName, replaceSuggestionCmd)
import LanguageServer.IdePurescript.Util.Position (convertRangePosition)
import LanguageServer.Protocol.Types (Command(..), DocumentUri(..))
import PscIde.Command (PscSuggestion(..), RebuildError(..))

topLevelDeclarationCodeLenses ∷
  Object (Array RebuildError) -> DocumentUri -> Aff (Array CodeLensResult)
topLevelDeclarationCodeLenses diagnostics uri = ado
  let fileDiagnostics = Object.lookup (un DocumentUri uri) diagnostics # fold
  in codeLenses uri fileDiagnostics

codeLenses ∷ DocumentUri -> Array RebuildError -> Array CodeLensResult
codeLenses docUri =
  mapMaybe case _ of
    RebuildError
    { errorCode: "MissingTypeDeclaration"
    , suggestion:
      Just
      ( PscSuggestion
        { replacement: signature, replaceRange: Just range }
      )
    } -> Just (mkCodeLensResult range signature)
    _ -> Nothing
  where
  mkCodeLensResult rangePosition signature = do
    let range = convertRangePosition rangePosition
    { range
    , command: Nullable.notNull (mkReplaceCommand signature range)
    , data: (Nullable.null # unsafeToForeign)
    }

  mkReplaceCommand signature range =
    Command
      { command: cmdName replaceSuggestionCmd
      , title: signature
      , arguments:
        Nullable.notNull
          [ unsafeToForeign docUri
          , unsafeToForeign signature
          , unsafeToForeign range
          ]
      }
