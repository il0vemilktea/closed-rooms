{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isHexDigit)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Fau.Compile (cacheKey)
import Fau.Csv
import Fau.Params
import Fau.Render
import Fau.Rewrite
import Test.Hspec

parseLines :: [Text] -> Either String [Param]
parseLines = parseParams "test.dsp" . T.unlines

rewriteLines :: [Text] -> [Text]
rewriteLines ls = either error (\ps -> T.lines (rewrite ps src)) (parseParams "test.dsp" src)
  where
    src = T.unlines ls

main :: IO ()
main = hspec $ do
    paramsSpec
    rewriteSpec
    cacheKeySpec
    settingsSpec
    statsSpec
    csvSpec
    sampleRateSpec

paramsSpec :: Spec
paramsSpec = describe "parseParams" $ do
    it "reads names, values, ranges and line numbers" $
        parseLines
            [ "import(\"stdfaust.lib\");"
            , "//@params"
            , "dur  = 40;   // seconds"
            , "gain = 0.3;  // [0, 1] level"
            , "//@end"
            , "process = 0;"
            ]
            `shouldBe` Right [Param "dur" 40 Nothing 3, Param "gain" 0.3 (Just (0, 1)) 4]

    it "accepts Faust number forms" $
        map paramValue
            <$> parseLines ["//@params", "a = 1.;", "b = .5;", "c = -2;", "d = 1.5e-3;", "//@end"]
            `shouldBe` Right [1, 0.5, -2, 1.5e-3]

    it "skips blank and comment lines inside the block" $
        map paramName
            <$> parseLines ["  // @params", "", "// the drone", "  base = 55;", "//@end"]
            `shouldBe` Right ["base"]

    it "gives no params to a file without a block" $
        parseLines ["process = 0;"] `shouldBe` Right []

    it "handles a last line without a newline" $
        parseParams "test.dsp" "//@params\nq = 14;\n//@end"
            `shouldBe` Right [Param "q" 14 Nothing 2]

    describe "rejects" $ do
        it "expressions in the block" $
            parseLines ["//@params", "x = 2 * y;", "//@end"] `shouldSatisfy` isLeft
        it "a block without //@end" $
            parseLines ["//@params", "q = 14;"] `shouldSatisfy` isLeft
        it "a name defined twice" $
            parseLines ["//@params", "q = 14;", "q = 15;", "//@end"] `shouldSatisfy` isLeft
        it "a value outside its range" $
            parseLines ["//@params", "gain = 1.5; // [0, 1]", "//@end"] `shouldSatisfy` isLeft
        it "a range with min > max" $
            parseLines ["//@params", "gain = 0.5; // [1, 0]", "//@end"] `shouldSatisfy` isLeft
        it "a malformed range" $
            parseLines ["//@params", "gain = 0.5; // [0, 1 level", "//@end"] `shouldSatisfy` isLeft
        it "a second block" $
            parseLines ["//@params", "//@end", "//@params", "//@end"] `shouldSatisfy` isLeft

rewriteSpec :: Spec
rewriteSpec = describe "rewrite" $ do
    it "turns params into controls and keeps every other line" $
        rewriteLines
            [ "import(\"stdfaust.lib\");"
            , "//@params"
            , "dur = 40;    // seconds"
            , "q   = 14;    // [1, 80] resonance"
            , "//@end"
            , "process = no.noise;"
            ]
            `shouldBe` [ "import(\"stdfaust.lib\");"
                       , "//@params"
                       , "dur = nentry(\"dur\", 0, -1e30, 1e30, 0);    // seconds"
                       , "q = nentry(\"q\", 1, 1, 80, 0);    // [1, 80] resonance"
                       , "//@end"
                       , "process = no.noise;"
                       , "fau_out = process : par(i, outputs(process), max(-1.0) : min(1.0));"
                       ]

    it "doesn't depend on default values" $
        rewriteLines ["//@params", "q = 14; // [1, 80]", "//@end"]
            `shouldBe` rewriteLines ["//@params", "q = 15; // [1, 80]", "//@end"]

    it "only adds the entry point to a file without params" $
        rewriteLines ["process = 0;"]
            `shouldBe` ["process = 0;", "fau_out = process : par(i, outputs(process), max(-1.0) : min(1.0));"]

cacheKeySpec :: Spec
cacheKeySpec = describe "cacheKey" $ do
    let key = cacheKey "toolchain" "process = 0;" ["lib/fau.lib\nnow = 1;"]

    it "is 16 hex digits" $
        key `shouldSatisfy` (\k -> T.length k == 16 && T.all isHexDigit k)

    it "changes with the toolchain, the source and the libraries" $ do
        cacheKey "other toolchain" "process = 0;" ["lib/fau.lib\nnow = 1;"] `shouldNotBe` key
        cacheKey "toolchain" "process = 1;" ["lib/fau.lib\nnow = 1;"] `shouldNotBe` key
        cacheKey "toolchain" "process = 0;" ["lib/fau.lib\nnow = 2;"] `shouldNotBe` key

settingsSpec :: Spec
settingsSpec = describe "settings" $ do
    let block = [Param "dur" 40 Nothing 1, Param "q" 14 (Just (1, 80)) 2]

    it "uses the defaults and the dur param without overrides" $
        settings block [] `shouldBe` Right ([Setting "dur" 40 False, Setting "q" 14 False], 40)

    it "applies overrides, the last one per name winning" $
        settings block [("q", 20), ("q", 30), ("dur", 5)]
            `shouldBe` Right ([Setting "dur" 5 True, Setting "q" 30 True], 5)

    it "takes dur from the command line for a piece without a dur param" $
        snd <$> settings [Param "q" 14 Nothing 1] [("dur", 3)] `shouldBe` Right 3

    it "renders 10 seconds without any dur" $
        snd <$> settings [] [] `shouldBe` Right 10

    describe "rejects" $ do
        it "an unknown name" $ settings block [("qq", 1)] `shouldSatisfy` isLeft
        it "a value outside the range" $ settings block [("q", 100)] `shouldSatisfy` isLeft
        it "a dur that isn't positive" $ settings block [("dur", 0)] `shouldSatisfy` isLeft

statsSpec :: Spec
statsSpec = describe "parseStats and warnings" $ do
    let soxStats dc pk =
            T.unlines
                [ "             Overall     Left      Right"
                , "DC offset   " <> dc <> "  0.000284  0.000284"
                , "Pk lev dB      " <> pk <> "     -5.23     -5.12"
                , "RMS lev dB    -20.02    -20.10    -19.94"
                ]

    it "reads the overall column" $
        parseStats (soxStats "0.000284" "-5.12") `shouldBe` Right (Stats (-5.12) (-20.02) 0.000284)

    it "has nothing to say about a normal render" $
        warnings <$> parseStats (soxStats "0.000284" "-5.12") `shouldBe` Right []

    it "warns about clipping, silence and DC offset" $ do
        length . warnings <$> parseStats (soxStats "0.000284" "0.00") `shouldBe` Right 1
        length . warnings <$> parseStats (soxStats "0.000000" "-inf") `shouldBe` Right 1
        length . warnings <$> parseStats (soxStats "0.120000" "-5.12") `shouldBe` Right 1

csvSpec :: Spec
csvSpec = do
    describe "csvToLib" $ do
        it "turns columns into tuples" $
            csvToLib "s.csv" (T.unlines ["freq,gain,q", "100,0.5,10", "200,0.3,12"])
                `shouldBe` Right (T.unlines ["// generated by fau from s.csv", "freq = (100, 200);", "gain = (0.5, 0.3);", "q = (10, 12);"])

        it "reads numpy's savetxt output" $
            csvToLib "s.csv" (T.unlines ["# freq,q", "1.000000000000000000e+02,1.200000000000000000e+01", ""])
                `shouldBe` Right (T.unlines ["// generated by fau from s.csv", "freq = (100);", "q = (12);"])

        describe "rejects" $ do
            it "an unnamed column (a pandas index)" $ csvToLib "s.csv" ",freq\n0,100\n" `shouldSatisfy` isLeft
            it "a value that isn't a number" $ csvToLib "s.csv" "freq\nnan\n" `shouldSatisfy` isLeft
            it "a row with too few values" $ csvToLib "s.csv" "freq,q\n100\n" `shouldSatisfy` isLeft
            it "a file without rows" $ csvToLib "s.csv" "freq\n" `shouldSatisfy` isLeft
            it "two columns with the same name" $ csvToLib "s.csv" "q,q\n1,2\n" `shouldSatisfy` isLeft

    describe "csvImports" $
        it "finds library imports of .csv files" $
            csvImports "a = library(\"x.csv\");\nb = library(\"fau.lib\");\nc = library(\"d/y.csv\");"
                `shouldBe` ["x.csv", "d/y.csv"]

sampleRateSpec :: Spec
sampleRateSpec = describe "declaredSampleRate" $ do
    let sr = declaredSampleRate "test.dsp" . T.unlines
    it "is absent by default" $ sr ["process = 0;"] `shouldBe` Right Nothing
    it "reads a declaration" $ sr ["declare samplerate \"44100\";", "process = 0;"] `shouldBe` Right (Just 44100)
    it "ignores other declarations" $ sr ["declare samplerates \"1\";", "declare name \"x\";"] `shouldBe` Right Nothing
    describe "rejects" $ do
        it "a value that isn't whole Hz" $ sr ["declare samplerate \"44.1k\";"] `shouldSatisfy` isLeft
        it "two declarations" $ sr ["declare samplerate \"44100\";", "declare samplerate \"48000\";"] `shouldSatisfy` isLeft
