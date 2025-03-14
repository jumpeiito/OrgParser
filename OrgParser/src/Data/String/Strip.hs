module Data.String.Strip
  (strip
  , orgTagsParse
  , orgTimeParse
  , orgDateParse
  )  where

import Data.Char
import Text.Parsec
import Control.Monad

type Parser = Parsec String ()

orgTagsParse :: Parser [String]
orgTagsParse =
  char ':' *> ( mainParse <|> return [])
  where
    inner     = many (noneOf " :\t\n") <* lookAhead (char ':')
    mainParse = (:) <$> inner <*> orgTagsParse

orgTimeParse :: Parser (Int, Int)
orgTimeParse = do
  hs <- read <$> (count 2 digit <* char ':')
  ms <- read <$> count 2 digit
  guard $ (0 <= hs && hs <= 24) && (0 <= ms && ms <= 59)
  return (hs, ms)

orgDateParse :: Parser (Int, Int, Int)
orgDateParse = do
  y <- read <$> (count 4 digit <* char '-')
  m <- read <$> (count 2 digit <* char '-')
  d <- read <$> count 2 digit
  guard $ (2024 <= y && y <= 2099)
  guard $ (1 <= m && m <= 12)
  guard $ (1 <= d && d <= 31)
  return (y, m, d)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
