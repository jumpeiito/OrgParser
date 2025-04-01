{-# LANGUAGE OverloadedStrings #-}
module Org.GoogleCalendar.Event
  (
    CalendarEvent (..)
  , AlmostEqual (..)
  , eventDefault
  , testEvent
  )
where

import  Data.Maybe              (fromMaybe)
import  Data.Aeson
import  Data.Aeson.Types hiding (parse)
import  Data.Time
import  Text.Parsec

data CalendarEvent =
  CalendarEvent { eventCreated     :: String
                , eventDescription :: Maybe String
                , eventEnd         :: Maybe UTCTime
                , eventEtag        :: String
                , eventIcalUID     :: String
                , eventID          :: String
                , eventStart       :: Maybe UTCTime
                , eventSummary     :: String
                , eventUpdated     :: String
                , eventLocation    :: Maybe String }
  deriving (Eq)

newtype AlmostEqual = AlmostEqual { runAlomost :: CalendarEvent }
  deriving (Show)

instance Eq AlmostEqual where
  AlmostEqual (CalendarEvent _ dsc en _ _ _ st sumry _ loc) ==
    AlmostEqual (CalendarEvent _ dsc' en' _ _ _ st' sumry' _ loc')
    = (dsc == dsc')
      && (en == en')
      && (st == st')
      && (sumry == sumry')
      && (loc == loc')

eventDefault :: CalendarEvent
eventDefault =
  CalendarEvent { eventCreated     = mempty
                , eventDescription = Nothing
                , eventEnd         = Nothing
                , eventEtag        = mempty
                , eventIcalUID     = mempty
                , eventID          = mempty
                , eventStart       = Nothing
                , eventSummary     = mempty
                , eventUpdated     = mempty
                , eventLocation    = Nothing }

testEvent :: CalendarEvent
testEvent =
  eventDefault { eventDescription = Just "test"
               , eventEnd = Just $ UTCTime (fromGregorian 2025 4 1) (secondsToDiffTime 0)
               , eventEtag = "test etag"
               , eventIcalUID = "testUID"
               , eventID = "testID"
               , eventStart = Just $ UTCTime (fromGregorian 2025 3 31) (secondsToDiffTime 0)
               , eventSummary = "test summary"
               , eventLocation = Just "京建労会館" }

instance FromJSON CalendarEvent where
  parseJSON (Object v) = CalendarEvent <$> (v .: "created")
                                       <*> (v .:? "description")
                                       <*> (toUTCTime <$> (v .: "end"))
                                       <*> (v .: "etag")
                                       <*> (v .: "iCalUID")
                                       <*> (v .: "id")
                                       <*> (toUTCTime <$> (v .: "start"))
                                       <*> (v .: "summary")
                                       <*> (v .: "updated")
                                       <*> (v .:? "location")
  parseJSON invalid    =
    prependFailure "parsing CalendarEvent failed, "
    (typeMismatch "Object" invalid)


instance ToJSON CalendarEvent where
  toJSON (CalendarEvent _ dsc en etag _ _ st smry _ loc) =
    case (timeObject <$> st <*> en) of
      Nothing -> error "CalendarEvent ToJSON instance error"
      Just (st', en') ->
        object [ "etag"        .= etag
               , "summary"     .= smry
               , "description" .= dsc
               , "location"    .= mempty `fromMaybe` loc
               , "start"       .= st'
               , "end"         .= en'
               ]

instance Show CalendarEvent where
  show (CalendarEvent _ _ e _ _ _ s summary _ _) =
    let st = mempty `fromMaybe` (show <$> s)
        en = mempty `fromMaybe` (show <$> e) in
    st ++ "->" ++ en ++ " : " ++ summary

instance Ord CalendarEvent where
  (CalendarEvent _ _ _ _ _ _ s1 _ _ _) `compare`
    (CalendarEvent _ _ _ _ _ _ s2 _ _ _) = s1 `compare` s2

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
