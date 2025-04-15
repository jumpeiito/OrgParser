{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Org.ParseText
  (
    tagsP
  , rangeP
  , yearP
  , monthP
  , dayP
  , hourP
  , minuteP
  , timeP
  , japaneseDayofWeekP
  , dateYMDP
  , timestampTypeP
  , timestampCoreP
  , timestampSingleP
  , timestampP
  , orgstarsP
  , todoP
  , titleP
  , otherP
  , makeUTC
  , manyTill'
  , defOther
  , lineParse
  , defTitle
  , mplusOther
  , TimestampType (..)
  , Timestamp
  , Title
  , Line (..)
  , LevelEQTitle (..)
  , Other
  )
where

import           GHC.Base           (Alternative)
import           Data.Time
import           Data.Maybe         (isJust, maybeToList, fromMaybe)
import           Data.Void
import           Data.Coerce
import           Data.Tagged
import qualified Data.Text           as Tx
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad
import           Data.Extensible
import           Control.Lens        hiding ((:>), noneOf)


data TimestampType = Normal | Scheduled | Deadline | Closed
  deriving (Show, Eq)

data Line = LL Title
          | LP (Text, Text)
          | LB
          | LO Other
          deriving (Show, Eq)

type Title = Record
  [ "label"      :> Text
  , "level"      :> Int
  , "todo"       :> Maybe Text
  , "tags"       :> [Text]
  , "timestamps" :> [Timestamp]
  , "paragraph"  :> Text
  , "properties" :> [(Text, Text)]
  , "location"   :> Text
  , "path"       :> Text ]

type Timestamp = Record
  [ "begin"    :> UTCTime
  , "datetype" :> TimestampType
  , "active"   :> Bool
  , "end"      :> Maybe UTCTime ]

type Other = Record
  [ "timestamps" :> [Timestamp]
  , "others"     :> [Text] ]

type Text      = Tx.Text
type Parser    = Parsec Void Text
type Time      = (Tagged "Hour" Int, Tagged "Minute" Int)
data LineBreak = LineBreak
newtype Link   = Link (Text, Maybe Text) deriving Show
newtype LevelEQTitle = LEQ Title

-- mkField "label level todo tags timestamps paragraph properties location path"
-- mkField "begin datetype active end"
-- mkField "others"

defTitle :: Title
defTitle = #label         @= mempty
           <: #level      @= 0
           <: #todo       @= Nothing
           <: #tags       @= mempty
           <: #timestamps @= mempty
           <: #paragraph  @= mempty
           <: #properties @= mempty
           <: #location   @= mempty
           <: #path       @= mempty
           <: nil

defOther :: Other
defOther = #timestamps @= mempty
           <: #others  @= mempty
           <: nil

mplusOther :: Other -> Title -> Title
mplusOther o t =
  let
    othersRefine  = Tx.stripEnd . Tx.concat . (^. #others)
    x1 = t  & #timestamps %~ (<> o ^. #timestamps)
    x2 = x1 & #paragraph  %~ (<> othersRefine o)
  in
    x2

changeSlots :: ASetter a1 b1 a2 b2 -> b2 -> a1 -> b1
changeSlots sym value tsmp = tsmp & sym .~ value

tagsP :: Parser [Text]
tagsP = do
  let tkn = some $ noneOf (" :\t\n" :: [Token Text])
  let sep = single ':'
  tagname <- sep *> (Tx.pack <$> tkn)
             <* lookAhead sep
  loop    <- try tagsP <|> return []
  return $ tagname : loop

rangeP :: (Read a, Ord a) =>
  Tagged "Max" a -> Tagged "Min" a -> Tagged "Count" Int -> Parser a
rangeP maxi mini c = do
  parsed <- read <$> count (unTagged c) digitChar
  guard (mini <= coerce parsed && coerce parsed <= maxi)
  return parsed

yearP   :: Parser (Tagged "Year" Integer)
monthP  :: Parser (Tagged "Month" Int)
dayP    :: Parser (Tagged "Day" Int)
hourP   :: Parser (Tagged "Hour" Int)
minuteP :: Parser (Tagged "Minute" Int)
yearP   = (Proxy `tagWith`) <$> rangeP 2099 2024 4
monthP  = (Proxy `tagWith`) <$> rangeP 12 1 2
dayP    = (Proxy `tagWith`) <$> rangeP 31 1 2
hourP   = (Proxy `tagWith`) <$> rangeP 23 0 2
minuteP = (Proxy `tagWith`) <$> rangeP 59 0 2

timeP :: Parser (Time, Maybe Time)
timeP = (,) <$> timeCore
            <*> Nothing `option` (Just <$> (single '-' *> timeCore))
  where
    sep = char ':'
    timeCore :: Parser Time
    timeCore = (,) <$> hourP <* sep <*> minuteP

japaneseDayofWeekP :: Parser (Token Text)
japaneseDayofWeekP =
  choice $ map single "月火水木金土日"

dateYMDP ::
  Parser (Tagged "Year" Integer, Tagged "Month" Int, Tagged "Day" Int)
dateYMDP = (,,) <$> yearP <* sep <*> monthP <* sep <*> dayP
  where
    sep = char '-'

timestampTypeP :: Parser TimestampType
timestampTypeP = Normal `option` anyP parsers
  where
    parsers = [ try (chunk k >> return t)
              | (k, t) <- zip ["SCHEDULED: ", "DEADLINE: ", "CLOSED: "]
                              [Scheduled, Deadline, Closed]]

timestampCoreP :: Parser Timestamp
timestampCoreP = do
  let sep = single ' '
  (y, m, d)     <- dateYMDP
  _             <- sep >> japaneseDayofWeekP
  ((h, mi), en) <- (sep >> timeP) <|> return ((0, 0), Nothing)
  return $
    #begin @= makeUTC y m d h mi
    <: #datetype @= Normal
    <: #active @= True
    <: #end    @= (uncurry (makeUTC y m d) <$> en)
    <: nil

timestampSingleP :: Parser Timestamp
timestampSingleP = changeSlots #active True <$> activeParser
                   <|> changeSlots #active False <$> inactiveParser
  where
    activeParser       = between (single '<') (single '>') timestampCoreP
    inactiveParser     = between (single '[') (single ']') timestampCoreP

timestampP :: Parser Timestamp
timestampP = do
  stampStyle <- timestampTypeP
  let anotherTimestamp = Just <$> (single '-' *> timestampSingleP)
  (ts1, ts2) <- (,) <$> timestampSingleP
                    <*> (Nothing `option` anotherTimestamp)
  let endtime = (ts1 ^. #end) `mplus` ((^. #begin) <$> ts2)
  return $ foldr ($) ts1 [ #datetype `changeSlots` stampStyle
                         , #end `changeSlots` endtime]

orgstarsP :: Parser Int
orgstarsP = length <$> someTill (single '*') (single ' ')

todoP :: Parser (Maybe Text)
todoP = Nothing `option` (kwd <* many (single ' '))
  where
    kwd = anyP $ [ Just <$> chunk k
                 | k <- [ "TODO" , "DONE" , "WAIT" , "PEND" ]]

indicateP :: Parser ()
indicateP = try indicate >> some (single ' ') >> return ()
  where
    numslash = some digitChar >> single '/' >> some digitChar
    indicate = between (single '[') (single ']') numslash

titleAttachment :: Parser (Maybe Timestamp, [Text])
titleAttachment = do
  ts <- Nothing `option` (Just <$> try timestampP <* many (single ' '))
  tg <- [] `option` try tagsP
  guard $ isJust ts || not (null tg)
  return (ts, tg)

titleP :: Parser Title
titleP = do
  stars  <- orgstarsP
  todo   <- todoP
  _      <- indicateP <|> return ()
  ttl    <- try (manyTill' anySingle titleAttachment)
            <|> (Tx.pack <$> many anySingle)
  (g, t) <- (Nothing, []) `option` titleAttachment
  return $
    #label         @= Tx.stripEnd ttl
    <: #level      @= stars
    <: #todo       @= todo
    <: #tags       @= t
    <: #timestamps @= maybeToList g
    <: #paragraph  @= mempty
    <: #properties @= mempty
    <: #location   @= mempty
    <: #path       @= mempty
    <: nil

propertyP :: Parser (Text, Text)
propertyP =
  try (string ":PROPERTIES:" >> return ("PROPERTIES", mempty))
  <|> try (string ":END:" >> return ("END", mempty))
  <|> (,) <$> pname <*> (some (single ' ') *> pval)
  where
    pname, pval, pnameCore :: Parser Text
    pnameCore = Tx.pack <$> some (noneOf [':', '\n'])
    pname = between (single ':') (single ':') pnameCore
    pval  = Tx.pack <$> ((:) <$> satisfy (/= ' ') <*> many anySingle)

linkP :: Parser Text
linkP = between (chunk "[[") (chunk "]]") linkCore
  where
    linkToken = Tx.pack <$> some (noneOf [']'])
    linkCore  = do
      url  <- linkToken
      guard $ "http" `Tx.isPrefixOf` url
      expr <- Nothing `option` (chunk "][" >> Just <$> linkToken)
      return $ Tx.concat ["<a href=\"", url, "\">"
                         , url `fromMaybe` expr, "</a>"]

linebreakP :: Parser LineBreak
linebreakP = chunk "# linebreak" >> return LineBreak

-- "CLOSED: [2025-03-24 月 11:58] SCHEDULED: <2025-03-24 月>"
otherP :: Parser Other
otherP = do
  let makeDef label' val = defOther & label' .~ [val]
  let loop another = (<>) another <$> otherP
  let literally = do
        other <- try $ someTill anySingle eof
        loop (#others `makeDef` Tx.pack other)
  let link' = do
        lk <- try (linkP <* many (single ' '))
        loop (#others `makeDef` lk)
  let timestamp' = do
        ts <- try (timestampP <* many (single ' '))
        loop (#timestamps `makeDef` ts)
  let withP = do
        other <- try $ manyTill' anySingle (timestamp' <|> link')
        loop (#others `makeDef` other)
  defOther `option` (timestamp' <|> link' <|> withP <|> literally)

lineParse :: Parser Line
lineParse = LO defOther `option` (ll <|> lp <|> lb <|> lo)
  where
    ll = LL <$> try titleP
    lp = LP <$> try propertyP
    lb = try linebreakP >> return LB
    lo = LO <$> try otherP

-- ---- Utility -----------------------------------------------
fromG ::
  Tagged "Year" Integer ->
  Tagged "Month" Int ->
  Tagged "Day" Int ->
  Day
fromG y m d = fromGregorian (untag y) (untag m) (untag d)

makeUTC ::
  Tagged "Year" Integer ->
  Tagged "Month" Int ->
  Tagged "Day" Int ->
  Tagged "Hour" Int ->
  Tagged "Minute" Int ->
  UTCTime
makeUTC y m d h mi = UTCTime (fromG y m d) dayOfSeconds
  where
    dayOfSeconds =
      secondsToDiffTime $ toInteger h * 3600 + toInteger mi * 60

manyTill' :: MonadParsec e s f => f Char -> f a -> f Tx.Text
manyTill' p pend = loop
  where
    loop = do { _ <- lookAhead pend; return mempty }
           <|> do { x <- p; xs <- loop; return (x `Tx.cons` xs)}

anyP :: Alternative f => [f a] -> f a
anyP (p:parsers) = foldl (<|>) p parsers
anyP [] = undefined

-- pt
--   :: (ShowErrorComponent e, Show a) =>
--      Parsec e Text a -> String -> IO ()
-- pt parser str = parser `parseTest` Tx.pack str

instance Eq LevelEQTitle where
  (LEQ t1) == (LEQ t2) = t1 ^. #level == t2 ^. #level
