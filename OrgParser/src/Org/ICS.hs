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
import  Data.Time
import  Data.Aeson
import  Data.Aeson.Types
import  Data.Maybe              (catMaybes, fromJust)
import  Data.String.Conversions (convertString)
import  Control.Monad           (forM_)
import  Control.Monad.Reader    (Reader, ask, runReaderT, liftIO)
import  Network.HTTP.Req
import  Org.Node                (orgFileNode, nodeToCalendarEvents, orgFile)
import  Org.GoogleCalendar.Client
import  Org.GoogleCalendar.Event
import  System.Environment      (getEnv)
import  System.Directory        (getModificationTime)
import  qualified GHC.IO.Encoding as Encoding

data CalendarEventEqual = CeeAlmost CalendarEvent
                        | CeeEdible CalendarEvent CalendarEvent
                        | CeeNot CalendarEvent
                        deriving (Show, Eq)

isEdible :: CalendarEventEqual -> Bool
isEdible (CeeEdible _ _) = True
isEdible _ = False

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
  Encoding.setLocaleEncoding Encoding.utf8
  c        <- clientFromFile
  aToken   <- aliveAccessToken `runReaderT` c
  gcalList <- getGoogleCalendarList `runReaderT` (aToken, c)
  oFile    <- orgFile
  oModify  <- getModificationTime oFile
                >>= return . ((9 * 3600) `addUTCTime`)
  events   <- liftIO (nodeToCalendarEvents <$> orgFileNode oFile)
  let gap = filter isEdible $ diffCalendarEvent events gcalList
  let gcalNewer = filter (newer oModify . eventUpdated) gcalList
  let h   = head gap
  -- liftIO $ print h
  -- replaceEvent h `runReaderT` (aToken, c)
  print oModify
  mapM_ print gcalNewer
  -- forM_ (diffCalendarEvent events gcalList) $ \gap -> do
  --   case gap of
  --     CeeEdible _ _ -> return ()
  --     g             -> liftIO $ putStrLn $ show g
  where
    newer _ (Left _)      = False
    newer mtime (Right u) = u > mtime

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

replaceEvent :: CalendarEventEqual -> WithAccessToken ()
replaceEvent (CeeEdible org gcal) = do
  aToken <- fst <$> ask
  let url = googleCalendarHttps /: (convertString $ eventID gcal)
  runReq defaultHttpConfig $ do
    res  <- req PUT url (ReqBodyJson org) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print $ (responseBody res :: CalendarEvent)
    liftIO $ putStrLn $ eventSummary org ++ " registered!"
replaceEvent _ = return ()

data Calendar = Calendar [CalendarEvent] (Maybe String) deriving (Show)

instance FromJSON Calendar where
  parseJSON (Object v) = Calendar <$> (v .: "items")
                                  <*> (v .:? "nextPageToken")
  parseJSON invalid    =
    prependFailure "parsing Calendar failed, "
    (typeMismatch "Object" invalid)

-- insertTe:load c:/Users/kkr0133/Documents/OrgParser/OrgParser/src/Org/ICS.hs:load c:/Users/kkr0133/Documents/OrgParser/OrgParser/src/Org/ICS.hsst :: IO ()
-- insertTest = do
--   c      <- clientFromFile
--   aToken <- aliveAccessToken `runReaderT` c
--   insertEvent testEvent `runReaderT` (aToken, c)

-- testNotPush :: IO ()
-- testNotPush = do
--   c <- clientFromFile
--   a <- aliveAccessToken `runReaderT` c
--   notPushedCalendarEvents `runReaderT` (a, c) >>= mapM_ print
