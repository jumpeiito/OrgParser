{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Org.Parse.Time
  (
    Time
  , TimestampType (..)
  , Timestamp
  , TimeFull
  , GeocodeUTC (..)
  , OrgTime (..)
  , timestampP
  --------------------
  , yearP
  , monthP
  , dayP
  , hourP
  , minuteP
  , secondP
  , japaneseDayofWeekP
  , dateYMDP
  , timestampCoreP
  , timestampSingleP
  , timestampTypeP
  )
where

import           Control.Lens            hiding ((:>), noneOf)
import           Control.Monad           (guard, mplus)
import           Data.Time
import           Data.Coerce             (coerce)
import           Data.Proxy
import           Data.Tagged
import           Data.Extensible
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Org.Parse.Utility

data TimestampType = Normal | Scheduled | Deadline | Closed
  deriving (Show, Eq)

type Timestamp = Record
  [ "begin"    :> UTCTime
  , "datetype" :> TimestampType
  , "active"   :> Bool
  , "end"      :> Maybe UTCTime ]

type Time            = (Tagged "Hour" Int, Tagged "Minute" Int)
type TimeFull        = (Tagged "Hour" Int, Tagged "Minute" Int, Tagged "Second" Int)
newtype GeocodeUTC   = GU UTCTime

instance Show GeocodeUTC where
  show (GU d) =
    let
      (y', m', d') = toGregorian $ utctDay d
      y2  = y' `mod` 100
    in
      show y2 ++ "/" ++ show m' ++ "/" ++ show d'

rangeP :: (Read a, Ord a) =>
  Tagged "Max" a -> Tagged "Min" a -> Tagged "Count" Int -> Parser a
rangeP maxi mini c = do
  parsed <- read <$> count (unTagged c) digitChar
  -- let miniP = coerce parsed :: (Read a, Ord a, Num a) => Tagged "Min" a
  guard (mini <= coerce parsed && coerce parsed <= maxi)
  return parsed

yearP   :: Parser (Tagged "Year" Integer)
monthP  :: Parser (Tagged "Month" Int)
dayP    :: Parser (Tagged "Day" Int)
hourP   :: Parser (Tagged "Hour" Int)
minuteP :: Parser (Tagged "Minute" Int)
secondP :: Parser (Tagged "Second" Int)
yearP   = (Proxy `tagWith`) <$> rangeP 2099 2024 4
monthP  = (Proxy `tagWith`) <$> rangeP 12 1 2
dayP    = (Proxy `tagWith`) <$> rangeP 31 1 2
hourP   = (Proxy `tagWith`) <$> rangeP 23 0 2
minuteP = (Proxy `tagWith`) <$> rangeP 59 0 2
secondP = (Proxy `tagWith`) <$> rangeP 59 0 2

class OrgTime a where
  timeP :: Parser a

instance OrgTime Time where
  timeP = (,) <$> hourP <* (single ':') <*> minuteP

instance OrgTime TimeFull where
  timeP = (,,) <$> hourP <* sep <*> minuteP <* sep <*> secondP
    where
      sep = single ':'

instance OrgTime (Time, Maybe Time) where
  timeP = (,) <$> t
              <*> Nothing `option` (Just <$> (single '-' *> t))
    where
      t = timeP :: Parser Time

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

_timestampTypeRefineP :: Parser TimestampType
_timestampTypeRefineP =
  try (chunk "SCHEDULED: "    >> return Scheduled)
  <|> try (chunk "DEADLINE: " >> return Deadline)
  <|> try (chunk "CLOSED: "   >> return Closed)
  <|> return Normal

timestampCoreP :: Parser Timestamp
timestampCoreP = do
  let sep = single ' '
  (y, m, d)     <- dateYMDP
  _             <- sep >> japaneseDayofWeekP
  ((h, mi), en) <- ((0, 0), Nothing) `option` (sep >> timeP)
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
  ts1 <- timestampSingleP
  ts2 <- Nothing `option` (Just <$> (single '-' *> timestampSingleP))
  let endtime = (ts1 ^. #end) `mplus` ((^. #begin) <$> ts2)
  return $ foldr ($) ts1 [ #datetype `changeSlots` stampStyle
                         , #end `changeSlots` endtime]
{-# INLINE timestampP #-}
