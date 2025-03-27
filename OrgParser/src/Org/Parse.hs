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

import Data.Time
import Data.Monoid
import Data.Maybe
import Text.Parsec
import Control.Monad

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
             | ParserLineBreak
             | ParserOther String
                   deriving (Show, Eq)

data RangeNum = Range Int Int Int

-- ---tags-----------------------------------------------------
orgTagsParse :: Parser Element
orgTagsParse = do
  tagname <- char ':'
             *> many1 (noneOf " :\t\n")
             <* lookAhead (char ':')
  ParserTags loop <- (try orgTagsParse <|> (return $ ParserTags []))
  return $ ParserTags (tagname:loop)
  -- ---tags-----------------------------------------------------

-- ---time-----------------------------------------------------
withRangeParse :: String -> Int -> Int -> Int -> Parser Int
withRangeParse _ len s' e' = do
  parsed <- read <$> count len digit
  guard $ judgeRange $ Range parsed s' e'
  return parsed

judgeRange :: RangeNum -> Bool
judgeRange (Range min' max' x)
  | min' <= x && x <= max' = True
  | otherwise = False

orgTimeParse :: Parser (Int, Int)
orgTimeParse = (,) <$> withRangeParse "hour"   2 0 23
                   <*> withRangeParse "minute" 2 0 59

orgDateYMDParse :: Parser (Int, Int, Int)
orgDateYMDParse = (,,) <$> withRangeParse "year"  4 2024 2099
                       <*> withRangeParse "month" 2 1 12
                       <*> withRangeParse "day"   2 1 31

orgDateCoreParse :: Parser UTCTime
orgDateCoreParse = do
  (y, m, d) <- orgDateYMDParse <* space <* anyChar
  (h, mi)   <-
    -- ((oneOf "月火水木金土日") >> space >> orgTimeParse) <|> return (0,0)
    try (space >> orgTimeParse) <|> return (0,0)
  return $ UTCTime (fromGregorian (toInteger y) m d) $ timeToDiffTime (h, mi)

orgTimeStampParse :: Parser Element
orgTimeStampParse = do
  type'         <- typeP
  (bool, time') <- anyHit [acore, icore]
  end'          <- endTime
  return $ ParserTimeStamp { begin    = time'
                           , datetype = type'
                           , active   = bool
                           , end      = end' }
  where
    acore'   = between (char '<') (char '>') orgDateCoreParse
    icore'   = between (char '[') (char ']') orgDateCoreParse
    acore    = (,) <$> return True  <*> acore'
    icore    = (,) <$> return False <*> icore'
    rangesep = many space >> char '-' >> many space
    schedule = string "SCHEDULED: " >> return Scheduled
    deadline = string "DEADLINE: "  >> return Deadline
    closed   = string "CLOSED: "    >> return Closed
    typeP    = anyHit (map try [schedule, deadline, closed]) <|> return Normal
    endTime  = try (rangesep >> (Just . snd) <$> acore) <|> return Nothing

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
  stars  <- orgStars
  todo'  <- orgTODO <|> return Nothing
  _      <- try (indicate >> many1 space) <|> return mempty
  title' <- titlep
  (g, t) <- stopper <|> return (ParserTags [], mempty)
  return $ ParserTitle { title      = title'
                       , level      = length stars
                       , todo       = todo'
                       , tags       = g
                       , timestamps = t
                       }
  where
    orgStars     = many1 (char '*') <* many1 space
    orgTODO      = try (Just <$> todokwds <* many1 space)
    todokwds     = choice $ map string ["TODO", "DONE", "WAIT", "PEND"]
    indicate     = char '[' >> many digit >>
               char '/' >> many digit >> char ']' >> return ()
    untilStopper = manyTill' anyToken
    stopper_time = do { time' <- orgTimeStampParse; return (ParserTags [], [time']) }
    stopper_tags = do { tags' <- orgTagsParse; return (tags', mempty) }
    stopper_both = do
      time' <- orgTimeStampParse <* many space
      tags' <- orgTagsParse
      return (tags', [time'])
    stopper      = anyHit $ map try [ stopper_both, stopper_time, stopper_tags ]
    titlep       = (try $ untilStopper stopper) <|>  many anyChar
    -- probabilities = map (try . coreF) [ prob_both, prob_time, prob_tags ]
    -- coreP = do
    --   anyHit probabilities <|> many anyChar
-- -- ---title----------------------------------------------------

-- -- ---property-------------------------------------------------
orgPropertyParse :: Parser Element
orgPropertyParse = do
  ParserProperty <$> ((,) <$> pname <*> pval)
  where
    pname = char ':' *> many1 (noneOf ":\n") <* char ':'
    pval  = many space >> ((:) <$> (satisfy (/= ' ')) <*> many anyToken)
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

-- -- ---lineBreak------------------------------------------------
orgLineBreakParse :: Parser Element
orgLineBreakParse = do
  string "# linebreak" >> return ParserLineBreak
-- -- ---lineBreak------------------------------------------------

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

orgLineBreakCoreParse :: Parser [Element]
orgLineBreakCoreParse = do
  (:) <$> orgLineBreakParse <*> return []

orgLineCoreParse :: Parser [Element]
orgLineCoreParse = do
  try orgTitleLineCoreParse
  <|> orgPropertyLineCoreParse
  <|> orgLineBreakCoreParse
  <|> orgOtherLineCoreParse

orgLineParse :: String -> Either ParseError [Element]
orgLineParse s = parse orgLineCoreParse "" s
-- -- ---line-----------------------------------------------------
mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

manyTill' :: Stream s m t =>
             ParsecT s u m a1 -> ParsecT s u m a2 -> ParsecT s u m [a1]
manyTill' p pend = loop
  where
    loop = do { _ <- lookAhead pend; return [] }
           <|> do { x <- p; xs <- loop; return (x:xs)}

