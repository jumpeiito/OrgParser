{-# LANGUAGE AllowAmbiguousTypes #-}
module Org.Parse
  (
  --   orgLineParse , OrgElement (..)
    Element (..)
  , OrgTimeStampType (..)
  , orgTagsParse
  , orgDateYMDParse
  , orgDateCoreParse
  , orgDateCoreParseRefine
  , orgTimeStampParse
  , orgTimeParse
  , orgTitleParse
  , orgLineParse
  -- , orgPropertyParse
  , orgLinkParse
  , orgTitleLineCoreParse
  -- , orgOtherLineCoreParse
  -- , timeToDiffTime
  , mktime
  )
where

import Data.Time
-- import Data.Proxy
import qualified Data.List as Dl
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
------tags-----------------------------------------------------
orgTagsParse :: Parser Element
orgTagsParse = do
  tagname <- char ':'
             *> many1 (noneOf " :\t\n")
             <* lookAhead (char ':')
  ParserTags loop <- try orgTagsParse <|> return (ParserTags [])
  return $ ParserTags (tagname:loop)
------tags-----------------------------------------------------

-- ---time-----------------------------------------------------
withRangeParse :: String -> Int -> Int -> Int -> Parser Int
withRangeParse _ len s' e' = do
  parsed <- read <$> count len digit
  guard $ judgeRange $ Range s' e' parsed
  return parsed

judgeRange :: RangeNum -> Bool
judgeRange (Range min' max' x)
  | min' <= x && x <= max' = True
  | otherwise = False

orgTimeParse :: Parser (Int, Int)
orgTimeParse = (,) <$> withRangeParse "hour"   2 0 23 <* char ':'
                   <*> withRangeParse "minute" 2 0 59

orgDateYMDParse :: Parser (Int, Int, Int)
orgDateYMDParse = (,,) <$> withRangeParse "year"  4 2024 2099 <* char '-'
                       <*> withRangeParse "month" 2 1 12 <* char '-'
                       <*> withRangeParse "day"   2 1 31

makeUTC y m d h mi = UTCTime (makeDay y m d) (makeTime h mi)
  where
    makeDay y = fromGregorian (toInteger y)
    makeTime h mi = timeToDiffTime (h, mi)

orgDateCoreParse :: Parser UTCTime
orgDateCoreParse = do
  -- <2025-04-17 木 10:00>-<2025-04-18 金 12:00>
  -- Orgファイルとしては以下の形式も許容されるが,対応しない
  -- <2025-04-17 木 10:00-12:00>-<2025-04-18 金 12:00-17:00>
  (y, m, d) <- orgDateYMDParse <* space <* anyChar
  (h, mi)   <-
    -- ((oneOf "月火水木金土日") >> space >> orgTimeParse) <|> return (0,0)
    try (space >> orgTimeParse) <|> return (0,0)
  return $ makeUTC y m d h mi

orgDateCoreParseRefine :: Parser (UTCTime, Maybe UTCTime)
orgDateCoreParseRefine = do
  (y, m, d)     <- orgDateYMDParse <* space <* anyChar
  ((h, mi), en) <- try both <|> try startOnly <|> try donthit
    -- ((oneOf "月火水木金土日") >> space >> orgTimeParse) <|> return (0,0)
  return ( makeUTC y m d h mi
         , uncurry (makeUTC y m d) <$> en)
  where
    -- <2025-04-17 木 10:00-12:00>
    both = do
      start' <- space *> orgTimeParse <* char '-'
      end'   <- Just <$> orgTimeParse
      return (start', end')
    -- <2025-04-17 木 10:00>
    startOnly = flip (,) Nothing <$> (space *> orgTimeParse)
    -- <2025-04-17 木>
    donthit   = return ((0, 0), Nothing)

orgTimeStampTypeParse :: Parser OrgTimeStampType
orgTimeStampTypeParse =
  anyHit (map try [schedule, deadline, closed]) <|> return Normal
  where
    schedule = string "SCHEDULED: " >> return Scheduled
    deadline = string "DEADLINE: "  >> return Deadline
    closed   = string "CLOSED: "    >> return Closed

orgTimeStampParse :: Parser Element
orgTimeStampParse = do
  -- ORGで許容されている
  -- <2025-04-02 水 10:00-12:00>-<2025-04-03 木 10:00-12:00>
  -- のような形式には対応しない。
  -- 対応する形式は,
  -- (1) <2025-04-02 水>
  -- (2) <2025-04-02 水 10:00>
  -- (3) <2025-04-02 水 10:00-17:00>
  -- (4) <2025-04-02 水 10:00>-<2025-04-03 木 17:00>
  -- のいずれかとする。
  type'               <- orgTimeStampTypeParse
  (b, (st', timeend)) <- anyHit [acore, icore]
  end'                <- endTime
  return $ ParserTimeStamp { begin    = st'
                           , datetype = type'
                           , active   = b
                           , end      = timeend `mplus` end' }
  where
    dateP s' e' = between (char s') (char e') orgDateCoreParseRefine
    acore       = (,) True  <$> dateP '<' '>'
    icore       = (,) False <$> dateP '[' ']'
    rangesep    = many space >> char '-' >> many space
    endTime     = try (rangesep >> Just . fst . snd <$> acore)
                  <|> return Nothing

timeToDiffTime :: (Int, Int) -> DiffTime
timeToDiffTime (h, m) =
  secondsToDiffTime $ toInteger h * 3600 + toInteger m * 60

anyHit :: [ParsecT s u m a] -> ParsecT s u m a
anyHit (p:parsers) = foldl (<|>) p parsers
anyHit _ = undefined

-- ---time-----------------------------------------------------

-- ---title----------------------------------------------------
orgStarsParse :: Parser Int
orgStarsParse = length <$> (many1 (char '*') <* many1 space)

orgTODOParse :: Parser (Maybe String)
orgTODOParse =
  let
    todokwds     = choice $ map string ["TODO", "DONE", "WAIT", "PEND"]
  in
    try (Just <$> todokwds <* many1 space) <|> return Nothing

orgIndicateParse :: Parser ()
orgIndicateParse =
  try (indicate >> many1 space >> return ()) <|> return ()
  where
    numslush = many1 digit >> char '/' >> many1 digit
    indicate = between (char '[') (char ']') numslush

orgTitleAttachmentParse :: Parser (Element, [Element])
orgTitleAttachmentParse = do
  timestamp <- (orgTimeStampParse <* many space) <|> return mempty
  tags      <- orgTagsParse <|> return (ParserTags [])
  return (tags, [timestamp])

orgTitleParse :: Parser Element
orgTitleParse = do
  stars  <- orgStarsParse
  todo'  <- orgTODOParse
  _      <- orgIndicateParse
  title' <- titlep
  (g, t) <- attach
  return $ ParserTitle { title      = title'
                       , level      = stars
                       , todo       = todo'
                       , tags       = g
                       , timestamps = t
                       }
  where
    attach = orgTitleAttachmentParse
    titlep = try (manyTill' anyToken attach) <|> many anyChar
    -- untilStopper = manyTill' anyToken
    -- stopper_time = do { time' <- orgTimeStampParse; return (ParserTags [], [time']) }
    -- stopper_tags = do { tags' <- orgTagsParse; return (tags', mempty) }
    -- stopper_both = do
    --   time' <- orgTimeStampParse <* many space
    --   tags' <- orgTagsParse
    --   return (tags', [time'])
    -- stopper      = anyHit $ map try [ stopper_both, stopper_time, stopper_tags ]
    -- titlep       = try (untilStopper stopper) <|>  many anyChar
-- -- ---title----------------------------------------------------

-- -- ---property-------------------------------------------------
orgPropertyParse :: Parser Element
orgPropertyParse = ParserProperty <$> ((,) <$> pname <*> pval)
  where
    pname = char ':' *> many1 (noneOf ":\n") <* char ':'
    pval  = many space >> ((:) <$> satisfy (/= ' ') <*> many anyToken)
-- -- ---property-------------------------------------------------

-- -- ---link-----------------------------------------------------
orgLinkParse :: Parser Element
orgLinkParse = try link1 <|> try link2
  where
    link1 = do
      link <- string "[[" *> many1 (satisfy (/= ']')) <* string "]]"
      guard $ "http" `Dl.isPrefixOf` link
      return (ParserLink link Nothing)
    link2 = do
      link <- string "[[" *> manyTill (satisfy (/= ']')) (string "]")
      guard $ "http" `Dl.isPrefixOf` link
      expl <- string "[" *> manyTill (satisfy (/= ']')) (string "]")
      return (ParserLink link (Just expl))
-- -- ---link-----------------------------------------------------

-- -- ---lineBreak------------------------------------------------
orgLineBreakParse :: Parser Element
orgLineBreakParse =
  string "# linebreak" >> return ParserLineBreak
-- -- ---lineBreak------------------------------------------------

-- -- ---line-----------------------------------------------------
orgTitleLineCoreParse :: Parser [Element]
orgTitleLineCoreParse = (:[]) <$> orgTitleParse
  -- elements <- sequence [title' ,timestamp', tags']
  -- return $ catMaybes elements
  -- where
  --   title'     = (Just <$> orgTitleParse)
  --   timestamp' = (Just <$> orgTimeStampParse <* many space) <|> return Nothing
  --   tags'      = (Just <$> orgTagsParse <|> return Nothing)

orgOtherLineCoreParse :: Parser [Element]
orgOtherLineCoreParse =
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
orgPropertyLineCoreParse =
  (:) <$> orgPropertyParse <*> return []

orgLineBreakCoreParse :: Parser [Element]
orgLineBreakCoreParse =
  (:) <$> orgLineBreakParse <*> return []

orgLineCoreParse :: Parser [Element]
orgLineCoreParse =
  try orgTitleLineCoreParse
  <|> orgPropertyLineCoreParse
  <|> orgLineBreakCoreParse
  <|> orgOtherLineCoreParse

orgLineParse :: String -> Either ParseError [Element]
orgLineParse = parse orgLineCoreParse ""
-- -- ---line-----------------------------------------------------
mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

manyTill' :: Stream s m t =>
             ParsecT s u m a1 -> ParsecT s u m a2 -> ParsecT s u m [a1]
manyTill' p pend = loop
  where
    loop = do { _ <- lookAhead pend; return [] }
           <|> do { x <- p; xs <- loop; return (x:xs)}
