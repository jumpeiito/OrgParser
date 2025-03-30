{-# LANGUAGE OverloadedStrings #-}
module Org.GoogleCalendar.Event
  (
    CalendarEvent (..)
  , eventDefault
  , testEvent
  )
where

import  Control.Monad
import  Data.Maybe              (fromMaybe)
import  Data.Aeson
import  Data.Aeson.Types hiding (parse)
import  Data.Time
import  Data.Time.Clock
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

testEvent =
  CalendarEvent { eventDescription = Just "test"
                , eventEnd = Just $ UTCTime (fromGregorian 2025 3 31)
                                            (secondsToDiffTime (17 * 3600))
                , eventEtag = "test etag"
                , eventIcalUID = "testUID"
                , eventID = "testID"
                , eventStart = Just $ UTCTime (fromGregorian 2025 3 31)
                                              (secondsToDiffTime (8 * 3600))
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

instance ToJSON CalendarEvent where
  toJSON (CalendarEvent _ dsc en etag uid id st smry _ loc) =
    object [ "etag"        .= etag
           , "summary"     .= smry
           , "description" .= dsc
           , "location"    .= mempty `fromMaybe` loc
           , "start"       .= googleTimeObject st
           , "end"         .= googleTimeObject en
           ]

instance Show CalendarEvent where
  show (CalendarEvent _ _ e _ _ _ s summary updated _) =
    let st = mempty `fromMaybe` (show <$> s)
        en = mempty `fromMaybe` (show <$> e) in
    st ++ "->" ++ en ++ " : " ++ summary

data EventTime = EventTime (Maybe String) (Maybe String) deriving (Show, Eq)

instance FromJSON EventTime where
  parseJSON (Object v) = EventTime <$> (v .:? "date")
                                   <*> (v .:? "dateTime")

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
               [let m = (show x) ++ "月" in (m, m) | x <- [1..12]]
             , dateTimeFmt    = ""
             , dateFmt        = ""
             , timeFmt        = ""
             , time12Fmt      = ""
             , knownTimeZones = []}

toUTCTime :: EventTime -> Maybe UTCTime
toUTCTime (EventTime d dt) =
  case parse dateParse "" <$> (d <> dt) of
    Nothing        -> Nothing
    Just (Right u) -> Just u
    _              -> Nothing

googleTimeFormat :: UTCTime -> String
googleTimeFormat utc
  | utctDayTime utc == 0 = formatTime jpTimeLocale "%Y-%m-%d" utc
  | otherwise = formatTime jpTimeLocale "%Y-%m-%dT%H:%M:%S" utc

googleTimeObject :: Maybe UTCTime -> Value
googleTimeObject Nothing = error "start or end time not set."
googleTimeObject (Just utc)
  | utctDayTime utc == 0 =
    object [ "date" .= googleTimeFormat utc
           , "timeZone" .= ("Asia/Tokyo" :: String)]
  | otherwise =
    object [ "dateTime" .= googleTimeFormat utc
           , "timeZone" .= ("Asia/Tokyo" :: String)]
