module IdePurescript.Spago where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Posix.Signal (Signal(..))
import Data.String (codePointFromChar)
import Data.String as String
import Data.String.Utils (lines)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign (Foreign, readString, unsafeToForeign)
import Foreign.Internal.Stringify (unsafeStringify)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.Types (DocumentStore, Settings)
import LanguageServer.Protocol.Window (showError)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Stream (onDataString)

addDependency ∷ DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
addDependency _ _ (ServerState { connection }) args = do
  case args of
    [ singleArg ]
      | Right (Right depName) <- runExceptT (readString singleArg) -> do
        { exit, stderr } <- spawnAffDefault "npx" [ "spago", "install", depName ]
        liftEffect case exit of
          Normally 0 -> pure (unsafeToForeign true)
          Normally 1 -> do
            for_ connection (flip showError $ "Failed to install: " <> stderr)
            pure (unsafeToForeign false)
          _ -> do
            for_ connection (flip showError $ "Failed to install even worse: " <> stderr)
            pure (unsafeToForeign false)
    _ -> do
      liftEffect
        $ for_ connection \c -> log c ("Invalid args to addDependency " <> unsafeStringify args)
      pure (unsafeToForeign false)

getPackages ∷ DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
getPackages _ _ _ _ = do
  { stdout } <- spawnAffDefault "npx" [ "spago", "ls", "packages" ]
  pure $ unsafeToForeign (stdout # lines <#> String.takeWhile (_ /= codePointFromChar ' '))

spawnAff ∷
  CP.SpawnOptions ->
  String ->
  Array String ->
  Aff
    { stdout ∷ String
    , stderr ∷ String
    , exit ∷ CP.Exit
    }
spawnAff options command arguments =
  makeAff \cb -> do
    stdoutRef <- Ref.new ""
    stderrRef <- Ref.new ""
    process <- CP.spawn command arguments options
    onDataString (CP.stdout process) UTF8 \string ->
      Ref.modify_ (_ <> string) stdoutRef
    onDataString (CP.stderr process) UTF8 \string ->
      Ref.modify_ (_ <> string) stderrRef
    CP.onError process $ cb <<< Left <<< CP.toStandardError
    CP.onExit process \exit -> do
      stdout <- Ref.read stdoutRef
      stderr <- Ref.read stderrRef
      cb <<< pure $ { stdout, stderr, exit }
    pure <<< effectCanceler <<< void $ CP.kill SIGTERM process

spawnAffDefault ∷
  String ->
  Array String ->
  Aff
    { exit ∷ CP.Exit
    , stderr ∷ String
    , stdout ∷ String
    }
spawnAffDefault = spawnAff defaultSpawnOptions
