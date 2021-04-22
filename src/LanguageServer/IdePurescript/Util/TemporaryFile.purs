module LanguageServer.IdePurescript.Util.TemporaryFile where

import Prelude

import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.String.Regex (regex) as R
import Data.String.Regex.Flags (global) as R
import Effect.Aff (Aff, apathize, bracket)
import IdePurescript.Regex (replace')
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (sep)

foreign import tmpDir :: Effect String

type Path = String

makeTemporaryFile :: Path -> String -> Aff Path
makeTemporaryFile fileName text = do
  dir <- liftEffect tmpDir
  uuid <- liftEffect genUUID
  let name = replace' (R.regex "[\\/\\\\:]" R.global) "-" fileName
      tmpFile = dir <> sep <> "ide-purescript-" <> show uuid <> "-" <> name
  FS.writeTextFile UTF8 tmpFile text
  pure tmpFile

withTemporaryFile :: forall a.
  Path -> String -> (Path -> Aff a)
  -> Aff a
withTemporaryFile fileName text action = bracket acquire cleanup run
  where
  acquire = makeTemporaryFile fileName text
  run tmpFile = action tmpFile
  cleanup = apathize <<< FS.unlink