module Data.String.Strip
  (strip
  , orgTagsParse
  , orgTimeParse
  , orgDateYMDParse
  , orgDateCoreParse
  , orgTitleParse
  , timeToDiffTime
  )  where

import Data.Char
import Data.Time
import Text.Parsec
import Control.Monad

type Parser = Parsec String ()

---tags---------------------------------------------------------------------------------------------
orgTagsParse :: Parser [String]
orgTagsParse =
  char ':' *> (mainParse <|> return [])
  where
    inner     = many (noneOf " :\t\n") <* lookAhead (char ':')
    mainParse = (:) <$> inner <*> orgTagsParse
---tags---------------------------------------------------------------------------------------------

---time---------------------------------------------------------------------------------------------
orgTimeParse :: Parser (Int, Int)
orgTimeParse = do
  [h, m] <- map read <$> sequence [ count 2 digit <* char ':'
                                  , count 2 digit]
  guard $ h `rangep` (0, 24) && m `rangep` (0, 59)
  return (h, m)

orgDateYMDParse :: Parser (Int, Int, Int)
orgDateYMDParse = do
  [y, m, d] <- map read <$> sequence [ count 4 digit <* char '-'
                                     , count 2 digit <* char '-'
                                     , count 2 digit]
  guard $
    y `rangep` (2024, 2099) && m `rangep` (1, 12) && d `rangep` (1, 31)
  return (y, m, d)

orgDateCoreParse :: Parser UTCTime
orgDateCoreParse = do
  (y, m, d) <- orgDateYMDParse <* space
  (h, mi)   <-
    -- ((oneOf "月火水木金土日") >> space >> orgTimeParse) <|> return (0,0)
    try (anyChar >> space >> orgTimeParse) <|> return (0,0)
  return $ UTCTime (fromGregorian (toInteger y) m d) $ timeToDiffTime (h, mi)

orgActiveDateParse, orgInactiveDateParse :: Parser UTCTime
orgActiveDateParse   = between (char '<') (char '>') orgDateCoreParse
orgInactiveDateParse = between (char '[') (char ']') orgDateCoreParse
-- SCHEDULED:
-- DEADLINE:
-- CLOSED:
rangep :: Int -> (Int, Int) -> Bool
rangep target (low, high)
  | low <= target && target <= high = True
  | otherwise                       = False

timeToDiffTime :: (Int, Int) -> DiffTime
timeToDiffTime (h, m) =
  secondsToDiffTime $ (toInteger h) * 3600 + (toInteger m) * 60
---time---------------------------------------------------------------------------------------------

---title--------------------------------------------------------------------------------------------
orgTitleParse :: Parser String
orgTitleParse = do
  stars <- many (char '*') <* many1 space
  todo  <- try (todokwds <* many1 space) <|> return mempty
  _     <- try (indicate >> many1 space) <|> return mempty
  title <- many anyChar
  return title
  where
    todokwds = choice $ map string ["TODO", "DONE", "WAIT", "PEND"]
    indicate = char '[' >> many digit >>
               char '/' >> many digit >> char ']' >> return ()
---title--------------------------------------------------------------------------------------------

---property-----------------------------------------------------------------------------------------
---property-----------------------------------------------------------------------------------------

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
