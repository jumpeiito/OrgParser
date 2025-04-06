{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Org.GoogleCalendar.Color
  (ColorSet (..))
where

import  Data.Aeson
import  qualified Data.Aeson.KeyMap as KM
import  Data.Aeson.Types hiding (parse)
import  qualified Data.HashMap.Strict as HashMap
-- data Color = Color { background :: String
--                    , foreground :: String }
--   deriving (Show, Eq)

-- -- data ColorPair = CP { pairKey :: String
-- --                     , pairColor :: Color}
-- --   deriving (Show, Eq)
-- data ColorPair = CP KeyMap Color
--   deriving (Show, Eq)

data ColorSet = ColorSet { calendar :: [String]
                         , event    :: [String] }
  deriving (Show, Eq)

-- instance FromJSON Color where
--   parseJSON (Object v) = Color <$> (v .: "background")
--                                <*> (v .: "foreground")
--   parseJSON invalid    =
--     prependFailure "parsing Color failed, "
--     (typeMismatch "Object" invalid)

-- instance FromJSON [ColorPair] where
--   parseJSON x =
--     let 
--       parseEntry :: (String, Value) -> Parser ColorPair
--       parseEntry (k, v) = undefined
--     in
--       parseJSON x >>= mapM parseEntry . HashMap.toList

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

-- instance ToJSON Color where
--   toJSON (Color b f) = object [ "background"  .= b
--                               , "foreground"  .= f ]

testDecode :: Maybe ColorSet
testDecode =
  decode "{\"calendar\": {\"1\": {\"background\": \"bg\", \"foreground\": \"fg\"}}}"
