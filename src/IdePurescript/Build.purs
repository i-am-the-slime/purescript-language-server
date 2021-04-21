module IdePurescript.Build where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (intercalate, uncons, (:))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (either, Either(..))
import Data.Maybe (maybe, Maybe(..))
import Data.String (Pattern(Pattern), indexOf, joinWith, split)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import IdePurescript.Exec (findBins, getPathVar)
import IdePurescript.PscErrors (PscResult(..), parsePscOutput)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding as Encoding
import Node.Process (getEnv)
import Node.Stream as S
import PscIde as P
import PscIde.Command (CodegenTarget, RebuildResult(..))
import PscIde.Server (Executable(Executable))

type BuildOptions =
  { command :: Command
  , directory :: String
  , useNpmDir :: Boolean
  }

data Command = Command String (Array String)

type BuildResult =
  { errors :: PscResult
  , success :: Boolean
  }

spawn :: BuildOptions -> Aff { cmdBins :: Array Executable, cp :: Maybe ChildProcess }
spawn { command: Command cmd args, directory, useNpmDir } = do
  pathVar <- liftEffect $ getPathVar useNpmDir directory
  cmdBins <- findBins pathVar cmd
  cp <- liftEffect $ case uncons cmdBins of
    Just { head: Executable cmdBin _ } -> do
      env <- liftEffect getEnv
      let childEnv = Object.insert "PATH" (either identity identity pathVar) env
      Just <$> CP.spawn cmdBin args (CP.defaultSpawnOptions { cwd = Just directory, env = Just childEnv })
    _ -> pure Nothing
  pure { cmdBins, cp }

build :: Notify -> BuildOptions -> Aff (Either String BuildResult)
build notify buildOptions@{ command: Command cmd args } = do
  { cmdBins, cp: cp' } <- spawn buildOptions
  makeAff $ \cb -> do
    let succ = cb <<< Right
        err = cb <<< Left
    notify Info $ "Resolved build command (1st is used): "
    traverse_ (\(Executable x vv) -> do
      notify Info $ x <> maybe "" (": " <> _) vv) cmdBins
    case cp' of
      Nothing -> succ $ Left $ "Didn't find command in PATH: " <> cmd
      Just cp -> do
        notify Info $ "Running build command: " <> intercalate " " (cmd : args)
        CP.onError cp (cb <<< Left <<< CP.toStandardError)
        errOutput <- Ref.new ""
        outOutput <- Ref.new ""
        let res :: Ref String -> String -> Effect Unit
            res r s = Ref.modify_ (_ <> s) r

        catchException err $ S.onDataString (CP.stderr cp) Encoding.UTF8 (res errOutput)
        catchException err $ S.onDataString (CP.stdout cp) Encoding.UTF8 (res outOutput)

        CP.onClose cp (\exit -> case exit of
          CP.Normally n | n == 0 || n == 1 -> do
            pursError <- Ref.read (errOutput)
            pursOutput <- Ref.read (outOutput)
            let lines = split (Pattern "\n") $ pursError <> pursOutput
                { yes: json, no: toLog } = Array.partition (\s -> indexOf (Pattern "{\"") s == Just 0) lines
            notify Info $ joinWith "\n" toLog
            case parsePscOutput <$> json of
              [ Left e ] -> succ $ Left $ "Couldn't parse build output: " <> e
              [ Right r ] -> succ $ Right { errors: r, success: n == 0 }
              [] -> succ $ Left "Problem running build: didn't find JSON output"
              _ -> succ $ Left "Found multiple lines of JSON output, don't know what to do"
          _ -> succ $ Left "Build process exited abnormally")
    pure mempty

rebuild :: Int -> String -> Maybe (Array CodegenTarget) -> Aff BuildResult
rebuild port file = rebuildWithTmpFile port file file

-- Same as rebuild but allows for specifying a different "actualFile"
rebuildWithTmpFile :: Int -> String -> String -> Maybe (Array CodegenTarget) -> Aff BuildResult
rebuildWithTmpFile port file actualFile targets = do
  res <- P.rebuild port file (Just actualFile) targets
  either
    (throwError <<< error)
    (pure <<< onResult)
    res
  where

  onResult :: Either RebuildResult RebuildResult -> BuildResult
  onResult =
    either (\errors -> { errors: PscResult { errors, warnings: [] }, success: true })
           (\warnings -> { errors: PscResult { errors: [], warnings }, success: true  })
    <<<
    bimap unwrap unwrap
    where
    unwrap (RebuildResult r) = r