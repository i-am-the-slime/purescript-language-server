module LanguageServer.IdePurescript.SignatureHelp.Types where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tree.Zipper as Zipper
import LanguageServer.Protocol.Handlers (SignatureHelp)
import LanguageServer.Protocol.Types (Position)

type State =
  Zipper.Loc
    { startPosition ∷ Position
    , maxPosition ∷ Position
    , functionName ∷ String
    , parameters ∷ NonEmptyArray String
    , previousResponse ∷ SignatureHelp
    }