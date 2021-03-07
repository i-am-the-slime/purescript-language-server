module LanguageServer.IdePurescript.SignatureHelp.Types where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tree.Zipper as Zipper
import LanguageServer.Handlers (SignatureHelp)
import LanguageServer.Types (Position)

type State =
  Zipper.Loc
    { startPosition ∷ Position
    , maxPosition ∷ Position
    , functionName ∷ String
    , parameters ∷ NonEmptyArray String
    , previousResponse ∷ SignatureHelp
    }