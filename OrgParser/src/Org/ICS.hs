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
import  System.Environment   (getEnv)
import  System.Directory     (getModificationTime)

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

getGoogleCalendarList :: WithAccessToken [CalendarEvent]
getGoogleCalendarList = sort <$> loop Nothing []
  where
    loop pageToken ret = do
      Calendar events np <- getGoogleCalendarPage pageToken
      case np of
        Just _  -> loop np (ret ++ events)
        Nothing -> return (ret ++ events)

getGoogleCalendarPage :: Maybe String -> WithAccessToken Calendar
getGoogleCalendarPage nextToken = do
  aToken <- fst <$> ask
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

-- notPushedCalendarEvents :: [CalendarEvent] -> WithAccessToken [CalendarEvent]
notPushedCalendarEvents :: [CalendarEvent] -> WithAccessToken ()
notPushedCalendarEvents events = do
  gcalList <- getGoogleCalendarList
  orgDir   <- liftIO $ getEnv "ORG"
  let orgFile = orgDir ++ "/notes.org"
  orgMtime <- liftIO $ getModificationTime orgFile
  liftIO $ print orgMtime

updateGoogleCalendar :: [CalendarEvent] -> WithAccessToken ()
updateGoogleCalendar events = forM_ withC $ \(c, e) -> do
  liftIO $ putStr $ show c ++ "/" ++ show len ++ " "
  insertEvent e
  where
    withC = zip [1..] events :: [(Int, CalendarEvent)]
    len = length events

insertEvent :: CalendarEvent -> WithAccessToken ()
insertEvent cal = do
  aToken <- fst <$> ask
  runReq defaultHttpConfig $ do
    res  <- req POST googleCalendarHttps (ReqBodyJson cal) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print $ (responseBody res :: CalendarEvent)
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
  c      <- clientFromFile
  aToken <- aliveAccessToken `runReaderT` c
  insertEvent testEvent `runReaderT` (aToken, c)
