{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module Org.GoogleCalendar.Event
  (
    CalendarEvent (..)
  , AlmostEqual (..)
  , EdibleEqual (..)
  , eventDefault
  , testEvent
  , Q (..)
  , ComposeString (..)
  , ComposeDate (..)
  , matchQuery
  , matchQueryOr
  )
where

import  Data.Maybe              (fromMaybe, fromJust, isNothing)
import  Data.Either             (fromRight, isLeft)
import  Data.Aeson
import  Data.Aeson.Types hiding (parse)
import  Data.Time
import  Data.Time.Calendar
import  Data.Time.Calendar.OrdinalDate
import  Data.List               (inits, tails)
import  qualified Data.Map.Strict as M
import  Text.Parsec
import  Data.Functor.Const

data CalendarEvent =
  CalendarEvent { eventCreated     :: Either ParseError UTCTime
                , eventDescription :: Maybe String
                , eventEnd         :: Maybe UTCTime
                , eventEtag        :: String
                , eventIcalUID     :: String
                , eventID          :: String
                , eventStart       :: Maybe UTCTime
                , eventSummary     :: String
                , eventUpdated     :: Either ParseError UTCTime
                , eventLocation    :: Maybe String
                , eventColorID     :: Maybe String }
  deriving (Eq)

newtype EdibleEqual = Edible { runEdible :: CalendarEvent }
  deriving (Show)
newtype AlmostEqual = Almost { runAlmost :: CalendarEvent }
  deriving (Show)

instance Eq EdibleEqual where
  Edible (CalendarEvent _ _ _ _ _ _ st sumry _ _ _) ==
    Edible (CalendarEvent _ _ _ _ _ _ st' sumry' _ _ _)
    = (sumry == sumry')
      && ((utctDay <$> st) == (utctDay <$> st'))

instance Eq AlmostEqual where
  Almost (CalendarEvent _ dsc en _ _ _ st sumry _ loc _) ==
    Almost (CalendarEvent _ dsc' en' _ _ _ st' sumry' _ loc' _)
    = (dsc == dsc')
      && (en == en')
      && (st == st')
      && (sumry == sumry')
      && (loc == loc')

eventDefault :: CalendarEvent
eventDefault =
  CalendarEvent { eventCreated     = Right (UTCTime (fromGregorian 2025 4 1)
                                                    (secondsToDiffTime 0))
                , eventDescription = Nothing
                , eventEnd         = Nothing
                , eventEtag        = mempty
                , eventIcalUID     = mempty
                , eventID          = mempty
                , eventStart       = Nothing
                , eventSummary     = mempty
                , eventUpdated     = Right (UTCTime (fromGregorian 2025 4 1)
                                                    (secondsToDiffTime 0))
                , eventLocation    = Nothing
                , eventColorID     = Nothing }

testEvent :: CalendarEvent
testEvent =
  eventDefault { eventDescription = Just "test"
               , eventEnd = Just $ UTCTime (fromGregorian 2025 4 1) (secondsToDiffTime 0)
               , eventEtag = "test etag"
               , eventIcalUID = "testUID"
               , eventID = "testID"
               , eventStart = Just $ UTCTime (fromGregorian 2025 3 31) (secondsToDiffTime 0)
               , eventSummary = "test summary"
               , eventLocation = Just "京建労会館"
               , eventColorID  = mempty }

instance FromJSON CalendarEvent where
  parseJSON (Object v) =
    let startParse = toUTCTime <$> (v .: "start") -- Parse (Maybe UTCTime)
        endParse   = toUTCTime <$> (v .: "end")   -- Parse (Maybe UTCTime)
        endTime    = repairTime <$> startParse <*> endParse
        oneDayS    = 24 * 60 * 60
        repairTime stMaybe enMaybe =
          let predicates = [(/= stMaybe), (> (addUTCTime oneDayS <$> stMaybe))] in
            case and (map ($ enMaybe) predicates) of
              True  -> UTCTime <$> (pred <$> utctDay <$> enMaybe)
                               <*> (utctDayTime <$> enMaybe)
              False -> enMaybe
    in
    CalendarEvent <$> (parse dateGreenWichParse "" <$> (v .: "created"))
                  <*> (v .:? "description")
                  <*> endTime
                  <*> (v .: "etag")
                  <*> (v .: "iCalUID")
                  <*> (v .: "id")
                  <*> startParse
                  <*> (v .: "summary")
                  <*> (parse dateGreenWichParse "" <$> (v .: "updated"))
                  <*> (v .:? "location")
                  <*> (v .:? "colorId")
  parseJSON invalid    =
    prependFailure "parsing CalendarEvent failed, "
    (typeMismatch "Object" invalid)

instance ToJSON CalendarEvent where
  toJSON (CalendarEvent _ dsc en etag _ _ st smry _ loc _) =
    case (timeObject <$> st <*> en) of
      Nothing -> error "CalendarEvent ToJSON instance error"
      Just (st', en') ->
        object [ "etag"        .= etag
               , "summary"     .= smry
               , "description" .= dsc
               , "location"    .= mempty `fromMaybe` loc
               , "start"       .= st'
               , "end"         .= en' ]

instance Show CalendarEvent where
  show (CalendarEvent _ d e _ _ _ s summary _ _ col) =
    let st = mempty `fromMaybe` (show <$> s)
        en = mempty `fromMaybe` (show <$> e) in
    st
    ++ "->"
    ++ en
    ++ " : "
    ++ summary
    ++ ":"
    ++ mempty `fromMaybe` col

instance Ord CalendarEvent where
  (CalendarEvent _ _ _ _ _ _ s1 _ _ _ _) `compare`
    (CalendarEvent _ _ _ _ _ _ s2 _ _ _ _) = s1 `compare` s2

data EventTime = EventTime (Maybe String) (Maybe String) deriving (Show, Eq)

instance FromJSON EventTime where
  parseJSON (Object v) = EventTime <$> (v .:? "date")
                                   <*> (v .:? "dateTime")
  parseJSON invalid    =
    prependFailure "parsing EventTime failed, "
    (typeMismatch "Object" invalid)

dateParse :: Parsec String () UTCTime
dateParse = do
  [y, m, d]  <- map read <$> sequence [ count 4 digit <* string "-"
                                      , count 2 digit <* string "-"
                                      , count 2 digit]
  [h, mi, s] <- (string "T" *> timeParse) <|> return [0, 0, 0]
  let day = fromGregorian (toInteger y) m d
  return $ UTCTime day (secondsToDiffTime (h * 3600 + mi * 60 + s))
  where
    timeParse = map read <$> sequence [ count 2 digit <* string ":"
                                      , count 2 digit <* string ":"
                                      , count 2 digit]

dateGreenWichParse :: Parsec String () UTCTime
dateGreenWichParse = ((9 * 3600) `addUTCTime`) <$> dateParse

jpTimeLocale :: TimeLocale
jpTimeLocale =
  TimeLocale { wDays          =
               [ ("日曜日", "日") , ("月曜日", "月") , ("火曜日", "火")
               , ("水曜日", "水") , ("木曜日", "木")
               , ("金曜日", "金") , ("土曜日", "土")]
             , months         =
               [let m = (show x) ++ "月" in (m, m) | x <- ([1..12] :: [Int])]
             , dateTimeFmt    = ""
             , dateFmt        = ""
             , timeFmt        = ""
             , time12Fmt      = ""
             , knownTimeZones = []
             , amPm           = (mempty, mempty)}

toUTCTime :: EventTime -> Maybe UTCTime
toUTCTime (EventTime d dt) =
  case parse dateParse "" <$> (d <> dt) of
    Nothing        -> Nothing
    Just (Right u) -> Just u
    _              -> Nothing

data GoogleTimeType = GDate | GDateTime deriving (Show, Eq)

googleTimeFormat :: UTCTime -> GoogleTimeType -> String
googleTimeFormat utc' GDate = formatTime jpTimeLocale "%Y-%m-%d" utc'
googleTimeFormat utc' GDateTime =
  formatTime jpTimeLocale "%Y-%m-%dT%H:%M:%S" utc'

makeTimeObject :: GoogleTimeType -> UTCTime -> Value
makeTimeObject GDate utc' =
  object [ "date" .= googleTimeFormat utc' GDate ]
makeTimeObject GDateTime utc' =
  object [ "dateTime" .= googleTimeFormat utc' GDateTime
         , "timeZone" .= ("Asia/Tokyo" :: String)]

timeObject :: UTCTime -> UTCTime -> (Value, Value)
timeObject st en
  | (utctDay st /= utctDay en) =
    let oneday = 24 * 60 * 60 in
    ( makeTimeObject GDate st
    , makeTimeObject GDate (oneday `addUTCTime` en))
  | utctDayTime st == 0 =
    (makeTimeObject GDate st, makeTimeObject GDate en)
  | otherwise =
    (makeTimeObject GDateTime st, makeTimeObject GDateTime en)

--------------------------------------------------
-- Query
--------------------------------------------------
-- data QueryContainType = QDescription String
--                       | QID String
--                       | QSummary String
--                       | QLocation String
--                       deriving (Show, Eq)

-- data QueryDateType = QCreated QTimeType
--                    | QStart QTimeType
--                    | QEnd QTimeType
--                    | QUpdated QTimeType
--                    deriving (Show, Eq)

-- class (Applicative f, Eq (f UTCTime)) =>
--   QueryDateMatch (f :: * -> *) where
--   failPred  :: f UTCTime -> Bool
--   unpure    :: f UTCTime -> UTCTime
--   dmatch    :: QTimeType -> f UTCTime -> Bool

--   QDate ds `dmatch` utc
--     | failPred utc == True = False
--     | otherwise   = datesetToUTCTime ds == unpure utc
--   QRange (d1, d2) `dmatch` utc
--     | failPred utc == True = False
--     | otherwise = datesetToUTCTime d1 <= unpure utc
--                     && datesetToUTCTime d2 >= unpure utc

-- instance QueryDateMatch Maybe where
--   failPred = isNothing
--   unpure   = fromJust

-- instance Eq a => QueryDateMatch (Either a) where
--   failPred = isLeft
--   unpure   = fromRight (UTCTime (fromGregorian 1900 1 1)
--                                 (secondsToDiffTime 0))

-- class QueryContains a where
--   translate :: a -> String
--   contains  :: String -> a -> Bool
--   contains [] _ = False
--   contains part s =
--     let subs = translate s in
--     part `elem` (inits subs ++ tails subs)

-- instance QueryContains String where
--   translate = id

-- instance QueryContains (Maybe String) where
--   translate = (mempty `fromMaybe`)

-- class Query a b c where
--   -- query :: b -> (CalendarEvent -> c) -> Bool
--   query    :: (b -> a) -> b -> CalendarEvent -> Bool
--   formula  :: (b -> a) -> (CalendarEvent -> c)
--   criteria :: b -> c -> Bool
--   query constructor val event =
--     criteria val ((formula constructor) event)

-- instance QueryContains a => Query QueryContainType String a where
--   formula QDescription = eventDescription
--   formula QID          = eventID
--   formula QSummary     = eventSummary
--   formula QLocation    = eventLocation

--   criteria = contains

-- instance QueryDateMatch f => Query QTimeType (f UTCTime) where
--   query = dmatch
  -- unQuery :: a -> b

-- instance Query QueryContainType where
--   q1 `query` e = 
-- query :: QueryType -> CalendarEvent -> Bool
-- query (QDescription d) e = d `contains` eventDescription e
-- query (QID d) e          = d `contains` eventID e
-- query (QSummary d) e     = d `contains` eventSummary e
-- query (QLocation d) e    = d `contains` eventLocation e
-- query (QCreated d) e  = d `dmatch` eventCreated e
-- query (QUpdated d) e  = d `dmatch` eventUpdated e
-- query (QStart d) e    = d `dmatch` eventStart e
-- query (QEnd d) e      = d `dmatch` eventEnd e

-- CalendarEvent -> Maybe String
-- CalendarEvent -> String
-- CalendarEvent -> Maybe UTCTime
-- CalendarEvent -> Either ParseError UTCTime
type DateSet = (Integer, Int, Int)

data Q = QContains String
       | QRegexp String
       | QDate Day
       | QRange DateSet DateSet
       | QYearMonth (Integer, Int)
       | QYear Integer
       | QMonth Int
       | QDay Int
       | QDayOfWeek DayOfWeek

class Applicative f => ComposeDate (f :: * -> *) where
  unpureDate    :: f UTCTime -> UTCTime
  isFailureDate :: f UTCTime -> Bool
  runDateQ      :: (CalendarEvent -> f UTCTime) -> Q -> CalendarEvent -> Bool
  (<+>)         :: (CalendarEvent -> f UTCTime) -> Q ->
                          (CalendarEvent -> Bool)

  runDateQ f q event
    | isFailureDate (f event) == True = False
    | otherwise =
      let pureday      = utctDay $ unpureDate $ f event
          greg         = toGregorian pureday
          (y1, m1, d1) = greg
      in
        case q of
          QDate d           -> d == pureday
          QRange d1 d2      -> (d1 <= greg) && (greg <= d2)
          QYear y           -> y == y1
          QMonth m          -> m == m1
          QDay d            -> d == d1
          QYearMonth (y, m) -> (y == y1) && (m == m1)
          QDayOfWeek dw     -> dw == dayOfWeek pureday
          _                 -> False

  f <+> q = runDateQ f q

class Applicative a => ComposeString (a :: * -> *) where
  unpureString     :: a String -> String
  isFailureString  :: a String -> Bool
  runStringQ       :: (CalendarEvent -> a String) -> Q ->
                         CalendarEvent -> Bool

  (<->) :: (CalendarEvent -> a String) -> Q -> (CalendarEvent -> Bool)

  runStringQ f q event
    | isFailureString (f event) == True = False
    | otherwise = case q of
                    QContains s -> s `contains` unpureString (f event)
                    _ -> False

  f <-> q = runStringQ f q

-- class (ComposeString f, ComposeDate f, Applicative f) =>
--   Compose f a where
--   unpure    :: f a -> a
--   isFailure :: f a -> Bool
--   predicate :: Q -> a -> Bool
--   runQ      :: (CalendarEvent -> f a) -> Q -> CalendarEvent -> Bool

--   runQ getter q event
--     | isFailure (getter event) == True = False
--     | otherwise = q `predicate` (unpure $ getter event)

instance ComposeDate Maybe where
  unpureDate = fromJust
  isFailureDate = isNothing

instance ComposeDate (Either ParseError) where
  unpureDate = fromRight (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0))
  isFailureDate = isLeft

instance ComposeString (Const String) where
  unpureString    = getConst
  isFailureString = (== mempty) . unpureString

instance ComposeString Maybe where
  unpureString    = fromJust
  isFailureString = isNothing

contains :: String -> String -> Bool
contains [] _ = False
contains part subs = part `elem` (inits subs ++ tails subs)

datesetToUTCTime :: DateSet -> UTCTime
datesetToUTCTime (y, m, d) =
  UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

datesetToDay :: DateSet -> Day
datesetToDay = utctDay . datesetToUTCTime
-- instance TestQuery (Maybe UTCTime)

matchQuery :: [CalendarEvent -> Bool] -> CalendarEvent -> Bool
matchQuery qs event = and $ map ($ event) qs

matchQueryOr :: [CalendarEvent -> Bool] -> CalendarEvent -> Bool
matchQueryOr qs event = or $ map ($ event) qs

test = [ (Const . eventSummary) <-> QContains "test"
       , eventStart <+> QDate (fromGregorian 2025 4 1)]
