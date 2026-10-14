-- | Project layout specification: root, @lib/@ and @build/@.
module Fau.Project (
    Project (..),
    findProject,
    findProjectFrom,
)
where

import Control.Monad (filterM)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

data Project = Project
    { projectRoot :: FilePath
    , projectLib :: FilePath
    -- ^ @lib/@, on Faust's import path
    , projectBuild :: FilePath
    -- ^ @build/@, for generated sources, binaries and renders
    }
    deriving (Eq, Show)

findProject :: FilePath -> IO Project
findProject dsp = makeAbsolute dsp >>= findProjectFrom . takeDirectory

findProjectFrom :: FilePath -> IO Project
findProjectFrom start = do
    root <- lookupEnv "FAU_ROOT" >>= maybe search pure
    pure Project{projectRoot = root, projectLib = root </> "lib", projectBuild = root </> "build"}
  where
    search = do
        dir <- makeAbsolute start
        fromMaybe dir . listToMaybe <$> filterM (\d -> doesFileExist (d </> "flake.nix")) (ancestors dir)
    ancestors d =
        let parent = takeDirectory d
         in d : if parent == d then [] else ancestors parent
