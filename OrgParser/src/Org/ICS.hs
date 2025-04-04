{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}
module Org.ICS
  (
    getGoogleCalendarList
  , updateGoogleCalendar
  , insertEvent
  )
where

import  Data.List               (sort, elemIndex)
import  Data.Text               (Text)
import  Data.Time
import  Data.Aeson
import  Data.Aeson.Types
import  Data.Maybe              (fromJust)
import  Data.String.Conversions (convertString)
import  Control.Monad           (forM_)
import  Control.Monad.Reader    (ask, runReaderT, liftIO)
import  Network.HTTP.Req
import  Org.Node                (orgFileNode, nodeToCalendarEvents, orgFile)
import  Org.GoogleCalendar.Client
import  Org.GoogleCalendar.Event
import  qualified Org.GoogleCalendar.Color as GCC
-- import  System.Directory        (getModificationTime)
import  qualified GHC.IO.Encoding as Encoding

data CalendarEventEqual = CeeAlmost CalendarEvent
                        | CeeEdible CalendarEvent CalendarEvent
                        | CeeNot CalendarEvent
                        deriving (Show, Eq)

isEdible :: CalendarEventEqual -> Bool
isEdible (CeeEdible _ _) = True
isEdible _ = False

isCeeNot :: CalendarEventEqual -> Bool
isCeeNot (CeeNot _) = True
isCeeNot _ = False

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

updateGoogleCalendar :: IO ()
updateGoogleCalendar = do
  Encoding.setLocaleEncoding Encoding.utf8
  c        <- clientFromFile
  aToken   <- aliveAccessToken `runReaderT` c
  (`runReaderT` (aToken, c)) $ do
    gcalList <- getGoogleCalendarList
    oFile    <- liftIO $ orgFile
    events   <- nodeToCalendarEvents <$> liftIO (orgFileNode oFile)
    let diffs    = diffCalendarEvent events gcalList
    forM_ (filter isEdible diffs) replaceEvent
    forM_ (filter isCeeNot diffs) $ \tobeceenot ->
      case tobeceenot of
        CeeNot s -> insertEvent s
        _        -> return ()
    -- forM_ events (liftIO . print)

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
    liftIO $ putStrLn $ eventSummary org ++ " replace!"
replaceEvent _ = return ()

data Calendar = Calendar [CalendarEvent] (Maybe String) deriving (Show)

instance FromJSON Calendar where
  parseJSON (Object v) = Calendar <$> (v .: "items")
                                  <*> (v .:? "nextPageToken")
  parseJSON invalid    =
    prependFailure "parsing Calendar failed, "
    (typeMismatch "Object" invalid)

searchCalendar :: IO ()
searchCalendar = do
  Encoding.setLocaleEncoding Encoding.utf8
  c        <- clientFromFile
  aToken   <- aliveAccessToken `runReaderT` c
  (`runReaderT` (aToken, c)) $ do
    gcalList <- getGoogleCalendarList
    oFile    <- liftIO $ orgFile
    events   <- nodeToCalendarEvents <$> liftIO (orgFileNode oFile)
    -- let diffs    = diffCalendarEvent events gcalList
    -- forM_ (filter isEdible diffs) replaceEvent
    -- forM_ (filter isCeeNot diffs) $ \(CeeNot c) -> insertEvent c
    let q = [ eventStart <+> QRange (2025, 4, 1) (2025, 4, 30)]
    forM_ (filter (matchQuery q) gcalList) $ liftIO . print
    forM_ (filter (matchQuery q) events)   $ liftIO . print

getColors :: IO ()
getColors = do
  c      <- clientFromFile
  aToken <- aliveAccessToken `runReaderT` c
  let url = https "www.googleapis.com"
            /: "calendar"
            /: "v3"
            /: "colors"
  runReq defaultHttpConfig $ do
    res  <- req GET url NoReqBody jsonResponse
              (headerAuthorization aToken)
    liftIO $ print $ (responseBody res :: GCC.ColorSet)
    -- liftIO $ putStrLn $ eventSummary org ++ " replace!"
-- GET https://www.googleapis.com/calendar/v3/colors
