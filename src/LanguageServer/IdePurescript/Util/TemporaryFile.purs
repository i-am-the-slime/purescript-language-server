module LanguageServer.IdePurescript.Util.TemporaryFile where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String.CodeUnits (stripSuffix)
import Data.String.Regex (regex) as R
import Data.String.Regex.Flags (global) as R
import Data.UUID (genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff, apathize, attempt, bracket, error, throwError)
import Effect.Class (liftEffect)
import Foreign.Internal.Stringify (unsafeStringify)
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
      tmpFile = dir <> sep <> "ide-purescript-" <> UUID.toString uuid <> "-" <> name
  FS.writeTextFile UTF8 tmpFile text
  pure tmpFile

withTemporaryFile :: forall a.
  Path ->

  String -> 
  (Path -> Aff a)
  -> Aff a
withTemporaryFile fileName text action = bracket acquire cleanup run
  where
  acquire = makeTemporaryFile fileName text
  run tmpFile = action tmpFile
  cleanup = apathize <<< FS.unlink

withTemporaryFFIFile :: forall a.
  Path ->
  String -> 
  ({ pursFilePath :: Path, jsFilePath :: Maybe Path } -> Aff a)
  -> Aff a
withTemporaryFFIFile fileName text action = do
  jsFileName <- maybe (throwError (error "File path does not end in .purs")) pure maybeJsFileName
  maybeFFIText <- FS.readTextFile UTF8 jsFileName # attempt
  withTemporaryFile fileName text \pursFilePath -> do
    case maybeFFIText of
      Right ffiText -> do
          jsFilePath <- maybe (throwError (error $ "makeTemporaryFile produces incorrect filenames: " <> unsafeStringify pursFilePath)) pure (toFFIName pursFilePath)
          bracket (acquire jsFilePath) cleanup \_ -> do
            FS.writeTextFile UTF8 jsFilePath ffiText
            action { pursFilePath, jsFilePath: Just jsFilePath }
        where
        acquire jsFilePath = FS.writeTextFile UTF8 jsFilePath text $> jsFilePath
        cleanup = apathize <<< FS.unlink
      Left _ ->
          action { pursFilePath, jsFilePath: Nothing }
  where
  toFFIName x = ado
    withoutExtension <- stripSuffix (Pattern ".purs") x
    in withoutExtension <> ".js"
  maybeJsFileName = toFFIName fileName