{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Compiling a piece into a render binary with fau's architecture file
(@arch/fau.cpp@, built into fau, needed for stuff like control values and custom preroll time).

Binaries are cached in @build/.bin@ under a hash key of everything used.
Changing values in a @param block in faust doesn't recompile tho.
-}
module Fau.Compile (
    Toolchain (..),
    toolchainFromEnv,
    Compiled (..),
    compile,
    diagram,
    Generated (..),
    generate,
    findCsv,
    csvFiles,
    cacheKey,
)
where

import Control.Exception (finally)
import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isHexDigit)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Foldable (for_)
import Data.List (nub, sort, stripPrefix)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Fau.Csv
import Fau.Params
import Fau.Project
import Fau.Rewrite
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, readProcess)
import Text.Printf (printf)

data Toolchain = Toolchain
    { faustInclude :: FilePath
    , sndfileInclude :: FilePath
    , sndfileLib :: FilePath
    , toolchainKey :: Text
    -- ^ identifies faust, the C++ compiler and libsndfile
    -- by store path (from an envvar def'd in the flake)
    }
    deriving (Eq, Show)

-- | The toolchain as set by the dev shell (and later the fau package):
-- @FAU_FAUST_INC@, @FAU_SNDFILE_INC@, @FAU_SNDFILE_LIB@ and @FAU_BUILD_KEY@.
toolchainFromEnv :: IO (Either String Toolchain)
toolchainFromEnv = do
    faustInc <- var "FAU_FAUST_INC"
    sndInc <- var "FAU_SNDFILE_INC"
    sndLib <- var "FAU_SNDFILE_LIB"
    key <- var "FAU_BUILD_KEY"
    pure (Toolchain <$> faustInc <*> sndInc <*> sndLib <*> (T.pack <$> key))
  where
    var name = maybe (Left (name <> " is not set; run fau inside the dev shell")) Right <$> lookupEnv name

data Compiled = Compiled
    { compiledBinary :: FilePath
    , compiledControls :: [Text]
    -- ^ the params the binary has a control for; Faust drops unused ones
    , compiledCached :: Bool
    -- ^ whether the binary was already there
    }
    deriving (Eq, Show)

faustFlags :: [String]
faustFlags = ["-double", "-rui", "-pn", T.unpack entryPoint]

architecture :: Text
architecture = $(makeRelativeToProject "arch/fau.cpp" >>= embedStringFile)

cxxFlags :: [String]
cxxFlags = ["-std=c++17", "-O3", "-w"]

compile :: Toolchain -> Project -> FilePath -> Text -> [Param] -> IO (Either String Compiled)
compile tc project dsp src params = runExceptT $ do
    g <- ExceptT (generate project dsp src params)
    libs <- liftIO (libraries [projectLib project, generatedPieceDir g])
    let name = takeBaseName dsp
        binDir = projectBuild project </> ".bin"
        key = T.unpack (cacheKey (toolchainKey tc) (generatedSource g) (libs <> generatedData g))
        bin = binDir </> name <> "-" <> key
        controls = bin <.> "controls"
    liftIO (createDirectoryIfMissing True binDir)
    cached <- liftIO ((&&) <$> doesFileExist bin <*> doesFileExist controls)
    unless cached $ do
        build tc dsp (generatedFile g) bin controls (includes project g)
        liftIO (prune binDir name key)
    names <- liftIO (T.lines . decodeUtf8 <$> BS.readFile controls)
    pure Compiled{compiledBinary = bin, compiledControls = names, compiledCached = cached}

-- | @faust -svg@
diagram :: Project -> FilePath -> Text -> [Param] -> IO (Either String FilePath)
diagram project dsp src params = runExceptT $ do
    g <- ExceptT (generate project dsp src params)
    let svgDir = projectBuild project </> takeBaseName dsp <> "-svg"
    liftIO (removePathForcibly svgDir)
    runFaust dsp (generatedFile g) (["-svg", "-pn", "process", "-O", projectBuild project] <> includes project g <> [generatedFile g])
    pure (svgDir </> "process.svg")

-- | rewritten source that's written to @build/.gen/NAME.dsp@.
data Generated = Generated
    { generatedFile :: FilePath
    , generatedSource :: Text
    , generatedPieceDir :: FilePath
    -- ^ the original's directory, where Faust looks for its imports
    , generatedData :: [Text]
    -- ^ the .lib files made from the CSV files it imports
    }

generate :: Project -> FilePath -> Text -> [Param] -> IO (Either String Generated)
generate project dsp src params = runExceptT $ do
    pieceDir <- liftIO (takeDirectory <$> makeAbsolute dsp)
    let genDir = projectBuild project </> ".gen"
        gen = genDir </> takeBaseName dsp <.> "dsp"
    libs <- traverse (csvLib pieceDir genDir) (csvImports src)
    let importing (path, out, _) = T.replace ("library(\"" <> path <> "\")") ("library(\"" <> T.pack out <> "\")")
        generated = foldr importing (rewrite params src) libs
    liftIO $ do
        createDirectoryIfMissing True genDir
        BS.writeFile gen (encodeUtf8 generated)
    pure
        Generated
            { generatedFile = gen
            , generatedSource = generated
            , generatedPieceDir = pieceDir
            , generatedData = [lib | (_, _, lib) <- libs]
            }
  where
    csvLib pieceDir genDir path = do
        file <-
            liftIO (findCsv project pieceDir path)
                >>= maybe (throwE (dsp <> ": can't find " <> T.unpack path <> " (looked next to the piece and in the project root)")) pure
        shown <- liftIO (makeRelativeToCurrentDirectory file)
        lib <- liftIO (decodeUtf8 <$> BS.readFile file) >>= either throwE pure . csvToLib shown
        let out = genDir </> "csv" </> takeBaseName file <> "-" <> take 8 (sha256Hex (T.pack file)) <.> "lib"
        liftIO $ do
            createDirectoryIfMissing True (takeDirectory out)
            BS.writeFile out (encodeUtf8 lib)
        pure (path, out, lib)

findCsv :: Project -> FilePath -> Text -> IO (Maybe FilePath)
findCsv project pieceDir path =
    listToMaybe <$> filterM doesFileExist [pieceDir </> T.unpack path, projectRoot project </> T.unpack path]

csvFiles :: Project -> FilePath -> Text -> IO [FilePath]
csvFiles project dsp src = do
    pieceDir <- takeDirectory <$> makeAbsolute dsp
    catMaybes <$> traverse (findCsv project pieceDir) (csvImports src)

includes :: Project -> Generated -> [String]
includes project g = ["-I", generatedPieceDir g, "-I", projectLib project]

runFaust :: FilePath -> FilePath -> [String] -> ExceptT String IO ()
runFaust dsp gen args = do
    (code, _, err) <- readProcess (proc "faust" args)
    let messages = T.replace (T.pack gen) (T.pack dsp) (decodeUtf8 (BL.toStrict err))
    unless (T.null messages) $ liftIO (BS.hPutStr stderr (encodeUtf8 messages))
    when (code /= ExitSuccess) $ throwE ("faust failed on " <> dsp)

build :: Toolchain -> FilePath -> FilePath -> FilePath -> FilePath -> [String] -> ExceptT String IO ()
build tc dsp gen bin controls includeFlags = ExceptT (runExceptT steps `finally` mapM_ removePathForcibly [cpp, tmp])
  where
    cpp = bin <.> "cpp"
    tmp = bin <.> "tmp"
    arch = takeDirectory gen </> "fau-arch.cpp"
    steps = do
        liftIO (BS.writeFile arch (encodeUtf8 architecture))
        runFaust dsp gen (faustFlags <> ["-a", arch] <> includeFlags <> [gen, "-o", cpp])
        code <- liftIO (decodeUtf8 <$> BS.readFile cpp)
        unless (hasNoInputs code) $ throwE (dsp <> ": process has inputs; fau only renders generators")
        liftIO (BS.writeFile controls (encodeUtf8 (T.unlines (controlNames code))))
        liftIO (hPutStrLn stderr ("compiling " <> dsp <> " ..."))
        let link =
                [ "-I" <> faustInclude tc
                , "-I" <> sndfileInclude tc
                , cpp
                , "-L" <> sndfileLib tc
                , "-Wl,-rpath," <> sndfileLib tc
                , "-lsndfile"
                , "-o"
                , tmp
                ]
        (cxxExit, _, cxxErr) <- readProcess (proc "c++" (cxxFlags <> link))
        when (cxxExit /= ExitSuccess) $ throwE ("c++ failed on " <> cpp <> ":\n" <> T.unpack (decodeUtf8 (BL.toStrict cxxErr)))
        liftIO (renamePath tmp bin)

cacheKey :: Text -> Text -> [Text] -> Text
cacheKey toolchain generated libs =
    T.pack . take 16 . sha256Hex $
        T.intercalate "\0" (toolchain : T.pack (unwords (faustFlags <> cxxFlags)) : architecture : generated : libs)

sha256Hex :: Text -> String
sha256Hex = concatMap (printf "%02x") . BS.unpack . SHA256.hash . encodeUtf8

libraries :: [FilePath] -> IO [Text]
libraries dirs = do
    files <- sort . concat <$> traverse libsIn (nub dirs)
    traverse (\f -> (\c -> T.pack f <> "\n" <> decodeUtf8 c) <$> BS.readFile f) files
  where
    libsIn dir = do
        exists <- doesDirectoryExist dir
        if exists
            then map (dir </>) . filter ((== ".lib") . takeExtension) <$> listDirectory dir
            else pure []

-- cleanup previous versions in @binDir@
prune :: FilePath -> String -> String -> IO ()
prune binDir name keep = do
    files <- listDirectory binDir
    for_ files $ \f -> case versionKey f of
        Just k | k /= keep -> removeFile (binDir </> f)
        _ -> pure ()
  where
    versionKey f = do
        rest <- stripPrefix (name <> "-") f
        let (k, ext) = splitAt 16 rest
        if length k == 16 && all isHexDigit k && ext `elem` ["", ".controls"] then Just k else Nothing

controlNames :: Text -> [Text]
controlNames code = map (T.takeWhile (/= '"')) (drop 1 (T.splitOn "addNumEntry(\"" code))

hasNoInputs :: Text -> Bool
hasNoInputs code =
    let rest = snd (T.breakOn "getNumInputs()" code)
     in not (T.null rest) && "return 0;" `T.isPrefixOf` T.stripStart (T.drop 1 (T.dropWhile (/= '{') rest))
