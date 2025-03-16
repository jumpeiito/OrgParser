module Data.String.Strip
  (strip
  , orgTagsParse
  , orgTimeParse
  , orgDateYMDParse
  , orgDateCoreParse
  , orgDateParse
  , orgTitleParse
  , orgPropertyParse
  , orgLinkParse
  , timeToDiffTime
  , OrgElement (..)
  )  where

import Data.Char
import Data.Time
import Data.Either
import Text.Parsec
import Control.Monad
import System.IO

-- class OrgElement a where
--   toString :: a -> String

-- instance OrgElement OrgTimeStamp where
--   toString (Active u)     = show u
--   toString (InActive u)   = show u
--   toString (Scheduled u)  = show u
--   toString (Deadline u)   = show u
--   toString (Closed u)     = show u
--   toString (Range (s, e)) = show s ++ "-" ++ show e

-- instance OrgElement OrgTitle where
--   toString o = title o

-- instance OrgElement OrgProperty where
--   toString PropBegin          = "<"
--   toString PropEnd            = ">"
--   toString (PropLocation s)   = s
--   toString (PropJoiner s)     = s
--   toString (PropOther (k, v)) = k ++ ": " ++ v

-- instance OrgElement OrgLine where
--   toString (OrgLine s) = s

-- instance OrgElement OrgOther where
--   toString (OrgOther s) = s

type Parser = Parsec String ()
type Destination = String
type Explanation = String

data TimeStamp
data Title
data Property
data Link

data OrgElement a = OrgTitle { title :: String
                             , level :: Int
                             , todo  :: Maybe String
                             , children :: [OrgElement Title]}
                  | Active UTCTime
                  | InActive UTCTime
                  | Scheduled UTCTime
                  | Deadline UTCTime
                  | Closed UTCTime
                  | Range (UTCTime, UTCTime)

                  | PropBegin
                  | PropEnd
                  | PropLocation String
                  | PropJoiner String
                  | PropOther (String, String)

                  | OrgLink Destination (Maybe Explanation)
                  | OrgOther String
                  deriving (Show, Eq)

-- data OrgTimeStamp = Active UTCTime
--                   | InActive UTCTime
--                   | Scheduled UTCTime
--                   | Deadline UTCTime
--                   | Closed UTCTime
--                   | Range (UTCTime, UTCTime)
--                   deriving (Show, Eq)

-- data OrgTitle = OrgTitle { title    :: String
--                          , level    :: Int
--                          , todo     :: Maybe String
--                          , children :: [OrgTitle]
--                          }
--               deriving (Show, Eq)

-- data OrgProperty = PropBegin
--                  | PropEnd
--                  | PropLocation String
--                  | PropJoiner String
--                  | PropOther (String, String)
--                  deriving (Show, Eq)

-- newtype OrgLine = OrgLine String deriving (Show, Eq)

-- data OrgLink = OrgLink Destination (Maybe Explanation) deriving (Show, Eq)

-- newtype OrgOther = OrgOther String deriving (Show, Eq)

---tags-----------------------------------------------------
orgTagsParse :: Parser [String]
orgTagsParse =
  char ':' *> (mainParse <|> return [])
  where
    inner     = many1 (noneOf " :\t\n") <* lookAhead (char ':')
    mainParse = (:) <$> inner <*> orgTagsParse
---tags-----------------------------------------------------

---time-----------------------------------------------------
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
  (y, m, d) <- orgDateYMDParse <* space <* anyChar
  (h, mi)   <-
    -- ((oneOf "月火水木金土日") >> space >> orgTimeParse) <|> return (0,0)
    try (space >> orgTimeParse) <|> return (0,0)
  return $ UTCTime (fromGregorian (toInteger y) m d) $ timeToDiffTime (h, mi)

orgDateParse :: Parser (OrgElement TimeStamp)
orgDateParse = do
  try range
    <|> try active
    <|> try inactive
    <|> try schedule
    <|> try deadline
    <|> try closed
  where
    acore    = between (char '<') (char '>') orgDateCoreParse
    icore    = between (char '[') (char ']') orgDateCoreParse
    rangesep = many space >> char '-' >> many space
    active   =    Active <$> acore
    inactive =  InActive <$> icore
    schedule = Scheduled <$> (string "SCHEDULED: " *> acore)
    deadline =  Deadline <$> (string "DEADLINE: "  *> acore)
    closed   =    Closed <$> (string "CLOSED: "    *> icore)
    range    = Range <$> ((,) <$> (acore <* rangesep) <*> acore)

rangep :: Int -> (Int, Int) -> Bool
rangep target (low, high)
  | low <= target && target <= high = True
  | otherwise                       = False

timeToDiffTime :: (Int, Int) -> DiffTime
timeToDiffTime (h, m) =
  secondsToDiffTime $ (toInteger h) * 3600 + (toInteger m) * 60
---time-----------------------------------------------------

---title----------------------------------------------------
orgTitleParse :: Parser (OrgElement Title)
orgTitleParse = do
  stars  <- many1 (char '*') <* many1 space
  todo'  <- try (Just <$> todokwds <* many1 space) <|> return Nothing
  _      <- try (indicate >> many1 space) <|> return mempty
  title' <- coreP
  return $ OrgTitle { title    = title'
                    , level    = length stars
                    , todo     = todo'
                    , children = [] }
  where
    todokwds = choice $ map string ["TODO", "DONE", "WAIT", "PEND"]
    indicate = char '[' >> many digit >>
               char '/' >> many digit >> char ']' >> return ()
    coreF    = manyTill' anyToken
    coreP    = do
      try (coreF (orgDateParse >> many space >> orgTagsParse))
      <|> try (coreF orgDateParse)
      <|> try (coreF orgTagsParse)
      <|> many anyChar
---title----------------------------------------------------

---property-------------------------------------------------
orgPropertyParse :: Parser (OrgElement Property)
orgPropertyParse = do
  try pbegin <|> try pend <|> try location <|> try joiner
    <|> PropOther <$> ((,) <$> otherPname <*> otherPval)
  where
    pbegin     = string ":PROPERTIES:" >> return PropBegin
    pend       = string ":END:"  >> return PropEnd
    pstr       = (:) <$> (satisfy (/= ':')) <*> many anyChar
    location   = PropLocation <$> (string ":LOCATION:" >> many space >> pstr)
    joiner     = PropJoiner <$> (string ":JOINER:" >> many space >> pstr)
    otherPname = char ':' *> many1 (noneOf ":\n") <* char ':'
    otherPval  = many space >> ((:) <$> (satisfy (/= ' ')) <*> many anyToken)
---property-------------------------------------------------

---link-----------------------------------------------------
orgLinkParse :: Parser (OrgElement Link)
orgLinkParse = do
  try link1 <|> try link2
  where
    link1 = do
      link <- string "[[" *> many1 (satisfy (/= ']')) <* string "]]"
      return (OrgLink link Nothing)
    link2 = do
      link <- string "[[" *> manyTill (satisfy (/= ']')) (string "]")
      expl <- string "[" *> manyTill (satisfy (/= ']')) (string "]")
      return (OrgLink link (Just expl))
---link-----------------------------------------------------

---line-----------------------------------------------------
orgLineParse :: String -> Either ParseError (OrgElement Title)
orgLineParse = parse orgTitleParse ""

-- orgLineCoreParse :: Parser [OrgElement a]
orgLineCoreParse = do
  try eof >> return []
  <|> try ((:) <$> orgTitleParse <*> orgLineCoreParse)
--   <|> try ((:) <$> orgTitleParse    <*> orgLineCoreParse)
  <|> try ((:) <$> orgDateParse     <*> orgLineCoreParse)
  -- <|> try ((:) <$> orgLinkParse     <*> orgLineCoreParse)
  -- <|> try ((:) <$> orgPropertyParse <*> orgLineCoreParse)
  -- <|> try ((:) <$> other            <*> orgLineCoreParse)
  -- where
  --   other = OrgOther <$> many1 anyChar
---line-----------------------------------------------------

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

manyTill' p end = loop
  where
    loop = do { _ <- lookAhead end; return [] }
           <|> do { x <- p; xs <- loop; return (x:xs)}

testParse :: IO ()
testParse = do
  contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
  mapM_ (putStrLn . show) $ rights $ map orgLineParse contents
