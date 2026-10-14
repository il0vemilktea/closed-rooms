{-# LANGUAGE OverloadedStrings #-}

{- | The basic idea is that I want to quickly preview .wav files
from Faust code and I don't want to write nentry here where
I will never "live-control" the sound.

So let's use faust constants but rewrite to nentries for compilation.
Then compiled binaries can be reused.
-}
module Fau.Rewrite (
    rewrite,
    entryPoint,
)
where

import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Fau.Params

-- | The definition to compile (@faust -pn@). This is going to be just
-- @process@ that's hard-clipped to [-1, 1] for libsndfile compatibility.
-- (libsndfile wraps samples rather than clipping)
entryPoint :: Text
entryPoint = "fau_out"

rewrite :: [Param] -> Text -> Text
rewrite params src = T.unlines (zipWith control [1 ..] (T.lines src) <> [clipped])
  where
    byLine = IntMap.fromList [(paramLine p, p) | p <- params]
    control n line = maybe line (nentry line) (IntMap.lookup n byLine)
    clipped = entryPoint <> " = process : par(i, outputs(process), max(-1.0) : min(1.0));"

-- | @name = nentry("name", init, min, max, 0);@ followed by the rest of the
-- original line. @init@ is a placeholder that will never really be used
-- unless you compile outside fau
nentry :: Text -> Param -> Text
nentry line p =
    paramName p <> " = nentry(\"" <> paramName p <> "\", " <> T.intercalate ", " args <> ");" <> rest
  where
    args = case paramRange p of
        Nothing -> ["0", "-1e30", "1e30", "0"]
        Just (lo, hi) -> [num lo, num lo, num hi, "0"]
    num = T.pack . showNumber
    rest = T.drop 1 (snd (T.breakOn ";" line))
