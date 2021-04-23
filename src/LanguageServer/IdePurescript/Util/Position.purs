module LanguageServer.IdePurescript.Util.Position where

import Prelude

import LanguageServer.Protocol.Types (Position(..), Range(..))
import PscIde.Command as Command

convertPosition ∷ Command.Position -> Position
convertPosition { line, column } =
  Position
    { line: line - 1
    , character: column - 1
    }

convertTypePosition ∷ Command.TypePosition -> Range
convertTypePosition (Command.TypePosition { start, end }) =
  Range
    { start: convertPosition start
    , end: convertPosition end
    }

convertRangePosition ∷ Command.RangePosition -> Range
convertRangePosition { startLine, startColumn, endLine, endColumn } = 
  Range
    { start: convertPosition { line: startLine, column: startColumn } 
    , end: convertPosition { line: endLine, column: endColumn }
    }