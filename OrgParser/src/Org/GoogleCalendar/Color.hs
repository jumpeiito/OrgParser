{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Org.GoogleCalendar.Color
  (ColorSet (..))
where

import  Data.Aeson
import  Data.Aeson.Types hiding (parse)
import  qualified Data.HashMap.Strict as HashMap

data ColorSet = ColorSet { colorsetCalendar :: [String]
                         , colorsetEvent    :: [String] }
  deriving (Show, Eq)

instance FromJSON ColorSet where
  parseJSON (Object v) =
    let
      calendar,event :: Parser (HashMap.HashMap Key Value)
      calendar = v .: "calendar"
      event    = v .: "event"
      calendarKeys = map show <$> HashMap.keys <$> calendar
      calendarEvents = map show <$> HashMap.keys <$> event
    in
      ColorSet <$> calendarKeys <*> calendarEvents
  parseJSON invalid    =
    prependFailure "parsing Color failed, "
    (typeMismatch "Object" invalid)

-- testDecode :: Maybe ColorSet
-- testDecode =
--   decode "{\"calendar\": {\"1\": {\"background\": \"bg\", \"foreground\": \"fg\"}}}"
