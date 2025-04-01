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

import  Data.List               (sort, elemIndex, (\\))
import  Data.Text               (Text)
import  Data.Aeson
import  Data.Aeson.Types
import  Data.Maybe              (catMaybes, fromJust)
import  Data.String.Conversions
import  Control.Monad
import  Control.Monad.Reader
import  Network.HTTP.Req
import  Org.GoogleCalendar.Client
import  Org.GoogleCalendar.Event
import  System.Environment   (getEnv)
import  System.Directory     (getModificationTime)
import  Org.Node             (orgFileNode, nodeToCalendarEvents)

data CalendarEventEqual = CeeAlmost CalendarEvent
                        | CeeEdible CalendarEvent CalendarEvent
                        | CeeNot CalendarEvent
                        deriving (Show, Eq)

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

-- notPushedCalendarEvents ::
--   [CalendarEvent] -> -- Org Events
--   [CalendarEvent] -> -- Google Calendar Events
--   [CalendarEvent]    -- Org Events that Google Calendar doesn't have
-- notPushedCalendarEvents orgEv gcalEv =
--   map runAlmost (map Almost orgEv \\ map Almost gcalEv)

-- orgNotHaveCalendarEvents ::
--   [CalendarEvent] -> -- Org Events
--   [CalendarEvent] -> -- Google Calendar Events
--   [CalendarEvent]    -- Google Calendar Events that org doesn't have
-- orgNotHaveCalendarEvents = flip notPushedCalendarEvents

diffCalendarEvent ::
  [CalendarEvent] ->    -- Org Events
  [CalendarEvent] ->    -- Google Calendar Events
  [CalendarEventEqual]  -- Org Events that Gcal doesn't have
diffCalendarEvent orgEv gcalEv = map judge orgEv
  where
    almost = map Almost gcalEv
    edible = map Edible gcalEv
    judge ev
      | (Almost ev `elem` almost) = CeeAlmost ev
      | (Edible ev `elem` edible) =
          let k = Edible ev `elemIndex` edible in
            CeeEdible ev (gcalEv !! fromJust k)
      | otherwise = CeeNot ev

testGap :: IO ()
testGap = do
  c        <- clientFromFile
  aToken   <- aliveAccessToken `runReaderT` c
  gcalList <- getGoogleCalendarList `runReaderT` (aToken, c)
  orgDir   <- liftIO $ getEnv "ORG"
  let orgFile = orgDir ++ "/notes.org"
  events   <- liftIO (nodeToCalendarEvents <$> orgFileNode orgFile)
  forM_ (diffCalendarEvent events gcalList) $ \gap -> do
    case gap of
      CeeAlmost _ -> return ()
      g           -> liftIO $ print g
-- notPushedCalendarEvents :: WithAccessToken [CalendarEvent]
-- notPushedCalendarEvents = do
--   gcalList <- getGoogleCalendarList
--   orgDir   <- liftIO $ getEnv "ORG"
--   let orgFile = orgDir ++ "/notes.org"
--   events   <- liftIO (nodeToCalendarEvents <$> orgFileNode orgFile)
--   let almostEvent = map AlmostEqual events
--   let almostGcal  = map AlmostEqual gcalList
--   let almosts = almostEvent \\ almostGcal
--   -- forM_ (almostGcal \\ almostEvent) $ liftIO . print
--   -- return $ map runAlomost almosts
--   return $ map runAlomost almosts

-- differOrgGcal :: [CalendarEvent] -> [CalendarEvent] -> [CalendarEvent]
-- differOrgGcal orgEV gcalEV =
--   let gap = map AlmostEqual orgEV \\ map AlmostEqual gcalEV
--       termP ev g = 

  -- orgMtime <- liftIO $ getModificationTime orgFile
  -- liftIO $ print orgMtime

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

-- insertTest :: IO ()
-- insertTest = do
--   c      <- clientFromFile
--   aToken <- aliveAccessToken `runReaderT` c
--   insertEvent testEvent `runReaderT` (aToken, c)

-- testNotPush :: IO ()
-- testNotPush = do
--   c <- clientFromFile
--   a <- aliveAccessToken `runReaderT` c
--   notPushedCalendarEvents `runReaderT` (a, c) >>= mapM_ print

