{-# LANGUAGE OverloadedStrings #-}

{- | Rendering a piece to a wav file.

* render for @dur@ seconds
* compile if necessary
* "preroll" the render without saving to settle slow filters
* measure the result with sox and report
-}
module Fau.Render (
    RenderOptions (..),
    Setting (..),
    settings,
    Stats (..),
    parseStats,
    warnings,
    Rendered (..),
    render,
    spectrogram,
)
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Fau.Compile
import Fau.Params
import Fau.Project
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension, takeBaseName, takeDirectory, takeFileName, (<.>), (</>))
import System.Process.Typed (proc, readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)

data RenderOptions = RenderOptions
    { overrides :: [(Text, Double)]
    -- ^ @name=value@ from the command line; the last one for a name wins
    , sampleRate :: Maybe Int
    -- ^ default: the piece's @declare samplerate@, else 48000
    , bitDepth :: Int
    , output :: Maybe FilePath
    -- ^ default: @build/NAME.wav@
    }
    deriving (Eq, Show)

-- | A param's value for one render.
data Setting = Setting
    { settingName :: Text
    , settingValue :: Double
    , settingOverridden :: Bool
    }
    deriving (Eq, Show)

-- | params with overrides (including duration)
settings :: [Param] -> [(Text, Double)] -> Either String ([Setting], Double)
settings params overs = do
    for_ (Map.toList given) $ \(name, value) ->
        case find ((== name) . paramName) params of
            Just p -> for_ (paramRange p) $ \(lo, hi) ->
                when (value < lo || value > hi) $
                    Left (T.unpack name <> "=" <> showNumber value <> " is outside its range [" <> showNumber lo <> ", " <> showNumber hi <> "]")
            Nothing
                | name == "dur" -> pure ()
                | otherwise -> Left ("no param named " <> T.unpack name <> "; params: " <> T.unpack (T.intercalate ", " (map paramName params)))
    let set p = case Map.lookup (paramName p) given of
            Just v -> Setting (paramName p) v True
            Nothing -> Setting (paramName p) (paramValue p) False
        dur = fromMaybe 10 (Map.lookup "dur" given <|> paramValue <$> find ((== "dur") . paramName) params)
    when (dur <= 0) $ Left ("dur must be positive, not " <> showNumber dur)
    pure (map set params, dur)
  where
    given = Map.fromList overs

-- | summary stats from @sox FILE -n stats@.
data Stats = Stats
    { peakDb :: Double
    , rmsDb :: Double
    , dcOffset :: Double
    }
    deriving (Eq, Show)

parseStats :: Text -> Either String Stats
parseStats out = Stats <$> field "Pk lev dB" <*> field "RMS lev dB" <*> field "DC offset"
  where
    field label = case [T.words rest | l <- T.lines out, Just rest <- [T.stripPrefix label l]] of
        ((w : _) : _) -> maybe (Left ("can't read " <> T.unpack label <> " from sox: " <> T.unpack w)) Right (number w)
        _ -> Left ("no " <> T.unpack label <> " in sox's stats")
    number "-inf" = Just (-1 / 0)
    number w = readMaybe (T.unpack w)

-- | sanity checks and warnings: silence, clipping, dc offset
warnings :: Stats -> [Text]
warnings s =
    ["output is silent" | isInfinite (peakDb s)]
        <> ["output hit 0 dBFS and was hard-clipped; lower the gain" | peakDb s >= -0.01]
        <> [ T.pack (printf "DC offset of %.4f; add fi.dcblockerat(5) at the end of the chain" (dcOffset s))
           | abs (dcOffset s) > 0.01
           ]

preroll :: Double
preroll = 2

data Rendered = Rendered
    { renderedFile :: FilePath
    , renderedSettings :: [Setting]
    , renderedSeconds :: Double
    , renderedSampleRate :: Int
    , renderedCached :: Bool
    -- ^ whether the binary was reused
    , renderedUnused :: [Text]
    -- ^ params Faust removed because the piece doesn't use them (except @dur@)
    , renderedStats :: Stats
    }
    deriving (Eq, Show)

render :: Toolchain -> Project -> FilePath -> Text -> [Param] -> RenderOptions -> IO (Either String Rendered)
render tc project dsp src params opts = runExceptT $ do
    (sets, dur) <- either throwE pure (settings params (overrides opts))
    declared <- either throwE pure (declaredSampleRate dsp src)
    let sr = fromMaybe 48000 (sampleRate opts <|> declared)
    compiled <- ExceptT (compile tc project dsp src params)
    let out = fromMaybe (projectBuild project </> takeBaseName dsp <.> "wav") (output opts)
        frames = round (dur * fromIntegral sr) :: Integer
        prerollFrames = round (preroll * fromIntegral sr) :: Integer
        present = (`elem` compiledControls compiled) . settingName
        values = concat [[T.unpack (settingName s), showNumber (settingValue s)] | s <- sets, present s]
        -- fau.lib assumes "fau_preroll" nentry
        clock = concat [["fau_preroll", showNumber preroll] | "fau_preroll" `elem` compiledControls compiled]
        args = [out, show sr, show frames, show prerollFrames, show (bitDepth opts)] <> values <> clock
    liftIO (createDirectoryIfMissing True (takeDirectory out))
    (code, binOut, binErr) <- readProcess (proc (compiledBinary compiled) args)
    when (code /= ExitSuccess) $
        throwE ("rendering failed:\n" <> T.unpack (decodeUtf8 (BL.toStrict (binOut <> binErr))))
    (soxCode, _, soxErr) <- readProcess (proc "sox" [out, "-n", "stats"])
    let soxOut = decodeUtf8 (BL.toStrict soxErr)
    when (soxCode /= ExitSuccess) $ throwE ("sox failed on " <> out <> ":\n" <> T.unpack soxOut)
    stats <- either throwE pure (parseStats soxOut)
    pure
        Rendered
            { renderedFile = out
            , renderedSettings = sets
            , renderedSeconds = dur
            , renderedSampleRate = sr
            , renderedCached = compiledCached compiled
            , renderedUnused = [settingName s | s <- sets, not (present s), settingName s /= "dur"]
            , renderedStats = stats
            }

-- | render spectrogram png next to the sound with sox
spectrogram :: FilePath -> IO (Either String FilePath)
spectrogram wav = do
    let png = replaceExtension wav "png"
        args = [wav, "-n", "remix", "-", "spectrogram", "-x", "1600", "-y", "513", "-t", takeFileName wav, "-o", png]
    (code, _, err) <- readProcess (proc "sox" args)
    pure $
        if code == ExitSuccess
            then Right png
            else Left ("sox failed on " <> wav <> ":\n" <> T.unpack (decodeUtf8 (BL.toStrict err)))
