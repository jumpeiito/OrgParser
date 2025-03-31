{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
module Org.ICS
  (
    getGoogleCalendarList
  , updateGoogleCalendar
  , insertEvent
  )
where

import  Data.List               (sort)
import  Data.Text               (Text)
import  Data.Aeson
import  Data.Aeson.Types
import  Data.String.Conversions
import  Control.Monad
import  Control.Monad.Reader
import  Network.HTTP.Req
import  Org.GoogleCalendar.Client
import  Org.GoogleCalendar.Event

googleCalendarHttps :: Url 'Https
googleCalendarHttps =
  https "www.googleapis.com"
         /: "calendar"
         /: "v3"
         /: "calendars"
         /: "primary"
         /: "events"

headerAuthorization :: String -> Option scheme
headerAuthorization atoken =
  header "Authorization" ("Bearer " <> convertString atoken)

getGoogleCalendarList :: WithClient [CalendarEvent]
getGoogleCalendarList = sort <$> loop Nothing []
  where
    loop pageToken ret = do
      Calendar events np <- getGoogleCalendarPage pageToken
      case np of
        Just _  -> loop np (ret ++ events)
        Nothing -> return (ret ++ events)

getGoogleCalendarPage :: Maybe String -> WithClient Calendar
getGoogleCalendarPage nextToken = do
  aToken <- aliveAccessToken
  runReq defaultHttpConfig $ do
    res <- req GET googleCalendarHttps NoReqBody jsonResponse
               (headerAuthorization aToken <> query)
    return $ responseBody res
      where
        options :: [(Text, String)]
        query   :: Option scheme
        pToken Nothing  = []
        pToken (Just k) = [ ("pageToken", k)]
        options         = [ ("maxResults", "100"), ("singleEvents", "true")]
                             <> pToken nextToken
        query           = foldMap (uncurry (=:)) options

updateGoogleCalendar :: [CalendarEvent] -> WithClient ()
updateGoogleCalendar events = forM_ withC $ \(c, e) -> do
  liftIO $ putStr $ show c ++ "/" ++ show len ++ " "
  insertEvent e
  where
    withC = zip [1..] events :: [(Int, CalendarEvent)]
    len = length events

insertEvent :: CalendarEvent -> WithClient ()
insertEvent cal = do
  aToken <- aliveAccessToken
  runReq defaultHttpConfig $ do
    _      <- req POST googleCalendarHttps (ReqBodyJson cal) ignoreResponse
                  (headerAuthorization aToken)
    liftIO $ putStrLn $ eventSummary cal ++ " registered!"

data Calendar = Calendar [CalendarEvent] (Maybe String) deriving (Show)

instance FromJSON Calendar where
  parseJSON (Object v) = Calendar <$> (v .: "items")
                                  <*> (v .:? "nextPageToken")
  parseJSON invalid    =
    prependFailure "parsing Calendar failed, "
    (typeMismatch "Object" invalid)

insertTest :: IO ()
insertTest = do
  c <- clientFromFile
  insertEvent testEvent `runReaderT` c
