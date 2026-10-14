{-# LANGUAGE OverloadedStrings #-}

{- | The @//\@params@ block of a .dsp file, e.g.

> //@params
> dur = 40;    // seconds
> q   = 14;    // [1, 80] resonance
> //@end

Inside the block, only blank lines, comments and @name = <number>;@ lines are
allowed. A comment that starts with @[min, max]@ puts the value range
into the substituted nentry.
-}
module Fau.Params (
    Param (..),
    parseParams,
    parseNumber,
    declaredSampleRate,
    showNumber,
)
where

import Control.Monad (join, void, when)
import Data.Bifunctor (first)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (for_)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Numeric (showFFloat)
import Text.Megaparsec
import Text.Megaparsec.Char

data Param = Param
    { paramName :: Text
    , paramValue :: Double
    , paramRange :: Maybe (Double, Double)
    , paramLine :: Int
    -- ^ 1-based line of the definition in the source file
    }
    deriving (Eq, Show)

type Parser = Parsec Void Text

-- | Parse the params block of a .dsp file with Megaparsec
parseParams :: FilePath -> Text -> Either String [Param]
parseParams path = first errorBundlePretty . parse dspFile path

-- | Look for sample rate as e.g. @declare samplerate "44100";@
declaredSampleRate :: FilePath -> Text -> Either String (Maybe Int)
declaredSampleRate path src = case declarations of
    [] -> Right Nothing
    [(n, value)]
        | not (T.null value) && T.all isDigit value && read (T.unpack value) > (0 :: Int) -> Right (Just (read (T.unpack value)))
        | otherwise -> Left (path <> ":" <> show n <> ": declare samplerate takes whole Hz, like \"44100\", not \"" <> T.unpack value <> "\"")
    _ -> Left (path <> ": samplerate is declared more than once")
  where
    declarations =
        [ (n, T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') rest)))
        | (n, line) <- zip [1 :: Int ..] (T.lines src)
        , Just rest <- [T.stripPrefix "declare samplerate" (T.strip line)]
        , T.null rest || T.head rest `elem` [' ', '\t', '"']
        ]

parseNumber :: Text -> Maybe Double
parseNumber = parseMaybe (number <* eof)

dspFile :: Parser [Param]
dspFile = before
  where
    before =
        ([] <$ eof)
            <|> (marker "@params" *> blockBody Set.empty <* after)
            <|> (restOfLine *> before)
    after =
        eof
            <|> (getOffset >>= \o -> marker "@params" *> failAt o "only one //@params block is allowed")
            <|> (restOfLine *> after)

blockBody :: Set Text -> Parser [Param]
blockBody seen = do
    done <- atEnd
    when done $ fail "the //@params block has no //@end"

    ([] <$ marker "@end") <|> (hspace *> (blankOrComment <|> definition))
  where
    blankOrComment = optional comment *> lineEnd *> blockBody seen
    definition = do
        p <- param seen
        (p :) <$> blockBody (Set.insert (paramName p) seen)

-- | @name = <number>;@ with maybe a trailing comment.
param :: Set Text -> Parser Param
param seen = do
    start <- getOffset
    line <- unPos . sourceLine <$> getSourcePos
    name <- identifier
    when (name `Set.member` seen) $
        failAt start ("param " <> T.unpack name <> " is defined twice")
    hspace *> void (char '=') *> hspace
    valueAt <- getOffset
    value <- number
    hspace *> void (char ';') *> hspace
    range <- join <$> optional (string "//" *> hspace *> optional range' <* comment')
    lineEnd
    for_ range $ \(lo, hi) ->
        when (value < lo || value > hi) $
            failAt valueAt (showNumber value <> " is outside its range [" <> showNumber lo <> ", " <> showNumber hi <> "]")
    pure Param{paramName = name, paramValue = value, paramRange = range, paramLine = line}
  where
    comment' = takeWhileP Nothing (/= '\n')

-- | @[min, max]@ in a comment *starting* with '['
range' :: Parser (Double, Double)
range' = do
    start <- getOffset
    _ <- char '[' <* hspace
    lo <- number <* hspace <* char ',' <* hspace
    hi <- number <* hspace <* char ']'
    when (lo > hi) $ failAt start "a range is [min, max] with min <= max"
    pure (lo, hi)

identifier :: Parser Text
identifier = label "param name" $ T.cons <$> satisfy isStart <*> takeWhileP Nothing isRest
  where
    isStart c = isAsciiLower c || isAsciiUpper c || c == '_'
    isRest c = isStart c || isDigit c

-- | Faust number literals: @1@, @1.@, @.5@, @-2@, @1.5e-3@.
number :: Parser Double
number = label "number" $ do
    negative <- option False (True <$ char '-')
    (int, frac) <- withInt <|> fracOnly
    ex <- option "0" (char' 'e' *> ((<>) <$> option "" ("-" <$ char '-' <|> "" <$ char '+') <*> digits))
    let x = read (T.unpack (int <> "." <> frac <> "e" <> ex))
    pure (if negative then negate x else x)
  where
    digits = takeWhile1P (Just "digit") isDigit
    withInt = (,) <$> digits <*> option "0" (char '.' *> option "0" digits)
    fracOnly = (,) "0" <$> (char '.' *> digits)

marker :: Text -> Parser ()
marker word = try (hspace *> string "//" *> hspace *> string word) *> restOfLine

comment :: Parser ()
comment = string "//" *> void (takeWhileP Nothing (/= '\n'))

restOfLine :: Parser ()
restOfLine = takeWhileP Nothing (/= '\n') *> lineEnd

lineEnd :: Parser ()
lineEnd = void eol <|> eof

failAt :: Int -> String -> Parser a
failAt offset msg = parseError (FancyError offset (Set.singleton (ErrorFail msg)))

showNumber :: Double -> String
showNumber x
    | x == fromIntegral r = show r
    | otherwise = showFFloat Nothing x ""
  where
    r = round x :: Integer
