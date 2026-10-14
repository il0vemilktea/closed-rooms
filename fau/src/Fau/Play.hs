-- | Playing wav files with an available player.
module Fau.Play (play) where

import Control.Monad (filterM, forever, void)
import Data.Maybe (isJust)
import System.Directory (findExecutable)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, runProcess)

-- | Play a wav file with afplay (macOS), pw-play (PipeWire), paplay
-- (PulseAudio) or sox's play, tried in order;
-- play forever if @loop@ is true;
-- stop on thread cancel
play :: Bool -> FilePath -> IO ()
play loop wav = do
    found <- filterM (fmap isJust . findExecutable . fst) players
    case found of
        [] -> hPutStrLn stderr "fau: no audio player found (afplay, pw-play, paplay or sox's play)"
        (cmd, args) : _ -> (if loop then forever else id) (void (runProcess (proc cmd (args <> [wav]))))
  where
    players = [("afplay", []), ("pw-play", []), ("paplay", []), ("play", ["-q"])]
