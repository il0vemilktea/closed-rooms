{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Fau.Compile
import Fau.Params
import Fau.Play
import Fau.Project
import Fau.Render
import Fau.Watch
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, makeRelativeToCurrentDirectory, removePathForcibly)
import System.Exit (die, exitFailure)
import System.FilePath (dropExtension, hasExtension, takeBaseName, takeDirectory, takeExtension, (<.>), (</>))
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Info (os)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (proc, runProcess_)
import Text.Printf (printf)

data Command
    = Params FilePath
    | Rewrite FilePath
    | Compile FilePath
    | Render FilePath RenderOptions Playback
    | Play FilePath Bool
    | Watch FilePath RenderOptions Bool
    | Diagram FilePath Bool
    | Spec FilePath Bool
    | New FilePath
    | Clean

data Playback = NoPlay | PlayOnce | PlayLoop

main :: IO ()
main = do
    -- emacs setup
    -- enforce run cleanup on SIGTERM
    tid <- myThreadId
    _ <- installHandler sigTERM (Catch (throwTo tid UserInterrupt)) Nothing
    -- keep std and err outs in step for emacs
    hSetBuffering stdout LineBuffering
    cmd <- execParser cli
    case cmd of
        Params file -> resolvePiece file >>= listParams
        Rewrite file -> resolvePiece file >>= printRewrite
        Compile file -> resolvePiece file >>= compileDsp
        Render file opts playback -> resolvePiece file >>= \f -> renderDsp f opts playback
        Play file loop -> resolvePiece file >>= \f -> playDsp f loop
        Watch file opts loop -> resolvePiece file >>= \f -> watchDsp f opts loop
        Diagram file open -> resolvePiece file >>= \f -> diagramDsp f open
        Spec file open -> resolvePiece file >>= \f -> specOf f open
        New name -> newPiece name
        Clean -> cleanBuild

cli :: ParserInfo Command
cli = info (commands <**> helper) (progDesc "render Faust pieces to wav")
  where
    commands =
        hsubparser $
            command "params" (info (Params <$> dspFile) (progDesc "list the //@params block of FILE.dsp"))
                <> command "rewrite" (info (Rewrite <$> dspFile) (progDesc "print the source fau compiles for FILE.dsp"))
                <> command "compile" (info (Compile <$> dspFile) (progDesc "compile FILE.dsp (or reuse the cached binary) and print its path"))
                <> command "render" (info (Render <$> dspFile <*> renderOptions <*> playback) (progDesc "render FILE.dsp to a wav file"))
                <> command "play" (info (Play <$> strArgument (metavar "FILE.dsp|FILE.wav|NAME") <*> loop) (progDesc "play the last render of FILE.dsp, or a wav file"))
                <> command "watch" (info (Watch <$> dspFile <*> renderOptions <*> loop) (progDesc "render and play FILE.dsp again whenever it or a library changes"))
                <> command "diagram" (info (Diagram <$> dspFile <*> open) (progDesc "draw the block diagram of FILE.dsp (SVG)"))
                <> command "spec" (info (Spec <$> strArgument (metavar "FILE.dsp|FILE.wav|NAME") <*> open) (progDesc "draw a spectrogram of the last render (PNG)"))
                <> command "new" (info (New <$> strArgument (metavar "NAME")) (progDesc "create src/NAME.dsp from a template"))
                <> command "clean" (info (pure Clean) (progDesc "remove build/"))
    dspFile = strArgument (metavar "FILE.dsp|NAME")
    loop = switch (long "loop" <> help "loop playback")
    open = not <$> switch (long "no-open" <> help "only print the path")
    playback =
        flag' PlayLoop (long "loop" <> help "play the result in a loop")
            <|> flag NoPlay PlayOnce (long "play" <> help "play the result")
    renderOptions =
        RenderOptions
            <$> many (argument override (metavar "NAME=VALUE..." <> help "override params from the //@params block"))
            <*> optional (option auto (long "sr" <> metavar "HZ" <> help "sample rate (default: the piece's declare samplerate, else 48000)"))
            <*> option depth (long "bd" <> metavar "BITS" <> value 24 <> showDefault <> help "bit depth: 16, 24 or 32")
            <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE" <> help "output file (default: build/NAME.wav)"))
    override = eitherReader $ \arg -> case T.breakOn "=" (T.pack arg) of
        (name, rest)
            | not (T.null name)
            , Just v <- parseNumber (T.drop 1 rest) ->
                Right (name, v)
        _ -> Left ("expected NAME=VALUE with a number, not " <> arg)
    depth = eitherReader $ \arg ->
        if arg `elem` ["16", "24", "32"] then Right (read arg) else Left "the bit depth is 16, 24 or 32"

-- | A file argument, which can also be a piece's name: @room-04@ is
-- @src/room-04.dsp@ in the project of the current directory.
resolvePiece :: FilePath -> IO FilePath
resolvePiece arg = do
    exists <- doesFileExist arg
    if exists || hasExtension arg
        then pure arg
        else do
            project <- findProjectFrom "."
            let file = projectRoot project </> "src" </> arg <.> "dsp"
            found <- doesFileExist file
            unless found $ die ("no file " <> arg <> ", and no piece " <> file)
            makeRelativeToCurrentDirectory file

-- | Parse and report errors (dont die)
loadDsp :: FilePath -> IO (Either String (Text, [Param]))
loadDsp file = do
    src <- decodeUtf8 <$> BS.readFile file
    pure ((,) src <$> parseParams file src)

-- | Parse and die on error
readDsp :: FilePath -> IO (Text, [Param])
readDsp file = loadDsp file >>= either die pure

listParams :: FilePath -> IO ()
listParams file = do
    (_, params) <- readDsp file
    let nameWidth = maximum (0 : map (T.length . paramName) params)
        valueWidth = maximum (0 : map (length . showNumber . paramValue) params)
    mapM_ (putStrLn . row nameWidth valueWidth) params
  where
    row nw vw p =
        T.unpack (T.justifyLeft nw ' ' (paramName p))
            <> "  "
            <> case paramRange p of
                Nothing -> val
                Just (lo, hi) -> pad vw val <> "  [" <> showNumber lo <> ", " <> showNumber hi <> "]"
      where
        val = showNumber (paramValue p)
    pad n s = s <> replicate (n - length s) ' '

printRewrite :: FilePath -> IO ()
printRewrite file = do
    (src, params) <- readDsp file
    project <- findProject file
    g <- generate project file src params >>= either die pure
    BS.putStr (encodeUtf8 (generatedSource g))

compileDsp :: FilePath -> IO ()
compileDsp file = do
    (src, params) <- readDsp file
    toolchain <- toolchainFromEnv >>= either die pure
    project <- findProject file
    compiled <- compile toolchain project file src params >>= either die pure
    putStrLn (compiledBinary compiled <> if compiledCached compiled then "  (cached)" else "")

renderDsp :: FilePath -> RenderOptions -> Playback -> IO ()
renderDsp file opts playback = do
    toolchain <- toolchainFromEnv >>= either die pure
    wav <- renderOnce toolchain file opts >>= maybe exitFailure pure
    case playback of
        NoPlay -> pure ()
        PlayOnce -> play False wav
        PlayLoop -> play True wav

renderOnce :: Toolchain -> FilePath -> RenderOptions -> IO (Maybe FilePath)
renderOnce toolchain file opts = do
    project <- findProject file
    result <- loadDsp file >>= either (pure . Left) (\(src, params) -> render toolchain project file src params opts)
    case result of
        Left err -> do
            hPutStrLn stderr err
            hPutStrLn stderr ("render failed: " <> file)
            pure Nothing
        Right r -> do
            out <- makeRelativeToCurrentDirectory (renderedFile r)
            let st = renderedStats r
            printf
                "%s -> %ss @ %d Hz / %d bit%s\n"
                file
                (showNumber (renderedSeconds r))
                (renderedSampleRate r)
                (bitDepth opts)
                (if renderedCached r then " (cached build)" else "" :: String)
            unless (null (renderedSettings r)) $
                putStrLn ("  " <> unwords [T.unpack (settingName s) <> "=" <> showNumber (settingValue s) <> (if settingOverridden s then "*" else "") | s <- renderedSettings r])
            printf "rendered %s  peak %.2f dBFS  rms %.2f dBFS  dc %.4f\n" out (peakDb st) (rmsDb st) (dcOffset st)
            for_ (renderedUnused r) $ \n -> putStrLn ("WARNING: param " <> T.unpack n <> " has no effect (the piece doesn't use it)")
            for_ (warnings st) $ \w -> putStrLn ("WARNING: " <> T.unpack w)
            pure (Just (renderedFile r))

playDsp :: FilePath -> Bool -> IO ()
playDsp file loop = wavFor file >>= play loop

wavFor :: FilePath -> IO FilePath
wavFor file = do
    wav <-
        if takeExtension file == ".wav"
            then pure file
            else (\p -> projectBuild p </> takeBaseName file <.> "wav") <$> findProject file
    exists <- doesFileExist wav
    unless exists $ die ("no render yet: " <> wav)
    pure wav

watchDsp :: FilePath -> RenderOptions -> Bool -> IO ()
watchDsp file opts loop = do
    toolchain <- toolchainFromEnv >>= either die pure
    project <- findProject file
    root <- canonicalizePath (projectRoot project)
    dsp <- canonicalizePath file
    libDirs <- traverse canonicalizePath [projectLib project, takeDirectory dsp]
    let ignored = map (root </>) ["build", ".git", "dist-newstyle"]
        -- the piece, a .lib where Faust looks for libraries, or a CSV it imports
        relevant changed = canonicalizePath changed >>= dependency
        dependency p
            | any (`isPrefixOf` p) ignored = pure False
            | p == dsp = pure True
            | takeExtension p == ".lib" = pure (takeDirectory p `elem` libDirs)
            | takeExtension p == ".csv" = do
                src <- decodeUtf8 <$> BS.readFile dsp
                elem p <$> (csvFiles project dsp src >>= traverse canonicalizePath)
            | otherwise = pure False
    watchFiles root relevant $ do
        putStrLn ("--- " <> file)
        renderOnce toolchain file opts >>= mapM_ (play loop)

diagramDsp :: FilePath -> Bool -> IO ()
diagramDsp file open = do
    (src, params) <- readDsp file
    project <- findProject file
    svg <- diagram project file src params >>= either die pure
    putStrLn svg
    when open (openInApp svg)

specOf :: FilePath -> Bool -> IO ()
specOf file open = do
    png <- wavFor file >>= spectrogram >>= either die pure
    putStrLn png
    when open (openInApp png)

openInApp :: FilePath -> IO ()
openInApp f = runProcess_ (proc (if os == "darwin" then "open" else "xdg-open") [f])

newPiece :: FilePath -> IO ()
newPiece arg = do
    let name = if takeExtension arg == ".dsp" then dropExtension arg else arg
    when ('/' `elem` name) $ die ("fau new takes a name, like room-04, not " <> arg)
    project <- findProjectFrom "."
    let file = projectRoot project </> "src" </> name <.> "dsp"
    exists <- doesFileExist file
    when exists $ die (file <> " already exists")
    createDirectoryIfMissing True (takeDirectory file)
    BS.writeFile file (encodeUtf8 template)
    putStrLn file
  where
    template =
        T.unlines
            [ "import(\"stdfaust.lib\");"
            , "fau = library(\"fau.lib\");"
            , ""
            , "//@params"
            , "dur  = 30;     // seconds (also the render length)"
            , "gain = 0.3;    // [0, 1]"
            , "base = 110;    // Hz"
            , "//@end"
            , ""
            , "process = os.osc(base) * gain * fau.fade(dur, 2, 5) <: _,_;"
            ]

cleanBuild :: IO ()
cleanBuild = do
    project <- findProjectFrom "."
    removePathForcibly (projectBuild project)
    putStrLn ("removed " <> projectBuild project)
