module Org.Parse
  (
  --   orgLineParse , OrgElement (..)
    Element (..)
  , OrgTimeStampType (..)
  , orgTagsParse
  , orgDateYMDParse
  , orgDateCoreParse
  , orgTimeStampParse
  , orgTimeParse
  , orgTitleParse
  , orgLineParse
  -- , orgPropertyParse
  -- , orgLinkParse
  -- , orgTitleLineCoreParse
  -- , orgOtherLineCoreParse
  -- , timeToDiffTime
  , mktime
  )
where

import Data.Char
import Data.Time
import Data.Maybe
import Data.Either
import Text.Parsec
import Control.Monad
import System.IO

type Parser = Parsec String ()
type Destination = String
type Explanation = String

data OrgTimeStampType = Normal | Scheduled | Deadline | Closed
  deriving (Show, Eq)

data Element = ParserTitle { title      :: String
                           , level      :: Int
                           , todo       :: Maybe String
                           , tags       :: Element   -- tags
                           , timestamps :: [Element]}
             | ParserTimeStamp { begin    :: UTCTime
                               , datetype :: OrgTimeStampType
                               , active   :: Bool
                               , end      :: Maybe UTCTime }
             | ParserTags [String]
             | ParserProperty (String, String)
             | ParserLink Destination (Maybe Explanation)
             | ParserOther String
                   deriving (Show, Eq)

-- ---tags-----------------------------------------------------
orgTagsParse :: Parser Element
orgTagsParse =
  char ':' *> (mainParse <|> return (ParserTags []))
  where
    inner     = many1 (noneOf " :\t\n") <* lookAhead (char ':')
    mainParse = do
      x               <- inner
      ParserTags loop <- orgTagsParse
      return $ ParserTags (x:loop)
-- ---tags-----------------------------------------------------

-- ---time-----------------------------------------------------
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

orgTimeStampParse :: Parser Element
orgTimeStampParse = do
  type'         <- anyHit (map try [schedule, deadline, closed]) <|> return Normal
  (bool, time') <- anyHit [acore, icore]
  end'          <- try (rangesep >> (Just . snd) <$> acore) <|> return Nothing
  return $ ParserTimeStamp { begin = time', datetype = type', active = bool, end = end' }
  where
    acore'   = between (char '<') (char '>') orgDateCoreParse
    icore'   = between (char '[') (char ']') orgDateCoreParse
    acore    = (,) <$> return True  <*> acore'
    icore    = (,) <$> return False <*> icore'
    rangesep = many space >> char '-' >> many space
    schedule = string "SCHEDULED: " >> return Scheduled
    deadline = string "DEADLINE: "  >> return Deadline
    closed   = string "CLOSED: "    >> return Closed

rangep :: Int -> (Int, Int) -> Bool
rangep target (low, high)
  | low <= target && target <= high = True
  | otherwise                       = False

timeToDiffTime :: (Int, Int) -> DiffTime
timeToDiffTime (h, m) =
  secondsToDiffTime $ (toInteger h) * 3600 + (toInteger m) * 60

anyHit :: [(ParsecT s u m a)] -> ParsecT s u m a
anyHit (p:parsers) = foldl (<|>) p parsers
anyHit _ = undefined

-- ---time-----------------------------------------------------

-- ---title----------------------------------------------------
orgTitleParse :: Parser Element
orgTitleParse = do
  stars  <- many1 (char '*') <* many1 space
  todo'  <- try (Just <$> todokwds <* many1 space) <|> return Nothing
  _      <- try (indicate >> many1 space) <|> return mempty
  title' <- titlep
  (g, t) <- probs <|> return (ParserTags [], mempty)
  return $ ParserTitle { title      = title'
                       , level      = length stars
                       , todo       = todo'
                       , tags       = g
                       , timestamps = t
                       }
  where
    todokwds = choice $ map string ["TODO", "DONE", "WAIT", "PEND"]
    indicate = char '[' >> many digit >>
               char '/' >> many digit >> char ']' >> return ()
    coreF    = manyTill' anyToken
    prob_time = do { time' <- orgTimeStampParse; return (ParserTags [], [time']) }
    prob_tags = do { tags' <- orgTagsParse; return (tags', mempty) }
    prob_both = do
      time' <- orgTimeStampParse <* many space
      tags' <- orgTagsParse
      return (tags', [time'])
    probs = anyHit $ map try [ prob_both, prob_time, prob_tags ]
    titlep = (try $ coreF probs) <|>  many anyChar
    probabilities = map (try . coreF) [ prob_both, prob_time, prob_tags ]
    coreP = do
      anyHit probabilities <|> many anyChar
-- -- ---title----------------------------------------------------

-- -- ---property-------------------------------------------------
orgPropertyParse :: Parser Element
orgPropertyParse = do
  ParserProperty <$> ((,) <$> otherPname <*> otherPval)
  where
    otherPname = char ':' *> many1 (noneOf ":\n") <* char ':'
    otherPval  = many space >> ((:) <$> (satisfy (/= ' ')) <*> many anyToken)
-- -- ---property-------------------------------------------------

-- -- ---link-----------------------------------------------------
orgLinkParse :: Parser Element
orgLinkParse = do
  try link1 <|> try link2
  where
    link1 = do
      link <- string "[[" *> many1 (satisfy (/= ']')) <* string "]]"
      return (ParserLink link Nothing)
    link2 = do
      link <- string "[[" *> manyTill (satisfy (/= ']')) (string "]")
      expl <- string "[" *> manyTill (satisfy (/= ']')) (string "]")
      return (ParserLink link (Just expl))
-- -- ---link-----------------------------------------------------

-- -- ---line-----------------------------------------------------
orgTitleLineCoreParse :: Parser [Element]
orgTitleLineCoreParse = do
  elements <- sequence [title' ,timestamp', tags']
  return $ catMaybes elements
  where
    title'     = (Just <$> orgTitleParse)
    timestamp' = (Just <$> orgTimeStampParse <* many space) <|> return Nothing
    tags'      = (Just <$> orgTagsParse <|> return Nothing)

orgOtherLineCoreParse :: Parser [Element]
orgOtherLineCoreParse = do
  (eof >> return [])
    <|> anyHit (map tryF parses)
    <|> tryF other
    <|> ((:) <$> orgOther <*> return [])
  where
    parses   = [orgTimeStampParse, orgLinkParse]
    other    = ParserOther <$> manyTill' anyToken (choice parses)
    tryF p   = try ((:) <$> p <*> orgOtherLineCoreParse)
    orgOther = try (ParserOther <$> many anyToken)

orgPropertyLineCoreParse :: Parser [Element]
orgPropertyLineCoreParse = do
  (:) <$> orgPropertyParse <*> return []

orgLineCoreParse :: Parser [Element]
orgLineCoreParse = do
  try orgTitleLineCoreParse
  <|> orgPropertyLineCoreParse
  <|> orgOtherLineCoreParse

orgLineParse :: String -> Either ParseError [Element]
orgLineParse s = parse orgLineCoreParse "" s
-- -- ---line-----------------------------------------------------

-- strip :: String -> String
-- strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

manyTill' :: Stream s m t =>
             ParsecT s u m a1 -> ParsecT s u m a2 -> ParsecT s u m [a1]
manyTill' p pend = loop
  where
    loop = do { _ <- lookAhead pend; return [] }
           <|> do { x <- p; xs <- loop; return (x:xs)}
