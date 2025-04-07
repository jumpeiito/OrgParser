{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
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
import  Control.Monad.Reader    (ask, runReaderT, liftIO, asks)
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
                        deriving (Eq)

instance Show CalendarEventEqual where
  show (CeeAlmost c) = "CeeAlmost " ++ show c
  show (CeeNot c) = "CeeNot " ++ show c
  show (CeeEdible c1 c2) = "CeeEdible\n" ++ show c1 ++ "\n" ++ show c2 ++ "\n"

data CalendarResponse =
  CalendarResponse [CalendarEvent] (Maybe String)
  deriving (Show)

instance FromJSON CalendarResponse where
  parseJSON (Object v) =
    CalendarResponse <$> (v .: "items")
                     <*> (v .:? "nextPageToken")
  parseJSON invalid    =
    prependFailure "parsing Calendar failed, "
    (typeMismatch "Object" invalid)

data Calendar = Calendar { calendarID  :: Text
                         , url         :: Url 'Https
                         , filterEvent :: CalendarEvent -> Bool }

makeCalendar :: String -> (CalendarEvent -> Bool) -> Calendar
makeCalendar key f =
  let keyText = convertString key in
    Calendar { calendarID = keyText
             , url = https "www.googleapis.com"
                           /: "calendar"
                           /: "v3"
                           /: "calendars"
                           /: keyText
                           /: "events"
             , filterEvent = f }

googleCalendar, googleFamilyCalendar :: Calendar
googleCalendar       = makeCalendar "primary" (not . isPersonal)
googleFamilyCalendar =
  makeCalendar "family11468855857577074545@group.calendar.google.com" isPersonal

isEdible :: CalendarEventEqual -> Bool
isEdible (CeeEdible _ _) = True
isEdible _ = False

isCeeNot :: CalendarEventEqual -> Bool
isCeeNot (CeeNot _) = True
isCeeNot _ = False

headerAuthorization :: String -> Option scheme
headerAuthorization atoken =
  header "Authorization" ("Bearer " <> convertString atoken)

accessTokenPair :: IO (String, Client)
accessTokenPair = do
  c <- clientFromFile
  a <- aliveAccessToken `runReaderT` c
  return (a, c)

getGoogleCalendarList :: Calendar -> WithAccessToken [CalendarEvent]
getGoogleCalendarList cal = sort <$> loop Nothing []
  where
    loop pageToken ret = do
      CalendarResponse events np <- getGoogleCalendarPage pageToken cal
      case np of
        Just _  -> loop np (ret ++ events)
        Nothing -> return (ret ++ events)

getGoogleCalendarPage ::
  Maybe String ->
  Calendar ->
  WithAccessToken CalendarResponse
getGoogleCalendarPage nextToken cal = do
  aToken <- asks fst
  runReq defaultHttpConfig $ do
    res <- req GET (url cal) NoReqBody jsonResponse
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
      | Almost ev `elem` almost = CeeAlmost ev
      | Edible ev `elem` edible =
          let
            k = Edible ev `elemIndex` edible
          in
            CeeEdible ev (gcalEv !! fromJust k)
      | otherwise = CeeNot ev

diffVerseCalendarEvent ::
  [CalendarEvent] -> -- Org Events
  [CalendarEvent] -> -- Google Calendar Events
  [CalendarEvent]    -- Google events that Org doesn't have
diffVerseCalendarEvent orgEV = filter judge
  where
    almost = map Almost orgEV
    judge ev
      | Almost ev `elem` almost = False
      | eventColorID ev == Just "11" = False -- Already Colored Events excepts
      | otherwise = True

updateGoogleCalendar :: Calendar -> IO ()
updateGoogleCalendar cal = do
  Encoding.setLocaleEncoding Encoding.utf8
  apair <- accessTokenPair
  (`runReaderT` apair) $ do
    gcalList <- getGoogleCalendarList cal
    allev    <- nodeToCalendarEvents <$> liftIO (orgFile >>= orgFileNode)
    let events    = filter (filterEvent cal) allev
    let diffs     = diffCalendarEvent events gcalList
    let diffVerse = diffVerseCalendarEvent events gcalList
    edibleEventsReplace cal diffs
    newEventsInsert cal diffs
    verseColored cal diffVerse

edibleEventsReplace :: Calendar -> [CalendarEventEqual] -> WithAccessToken ()
edibleEventsReplace cal events =
  forM_ (filter isEdible events) $ replaceEvent cal

newEventsInsert :: Calendar -> [CalendarEventEqual] -> WithAccessToken ()
newEventsInsert cal events =
  forM_ (filter isCeeNot events) $ \case
     CeeNot s -> insertEvent cal s
     _        -> return ()

verseColored :: Calendar -> [CalendarEvent] -> WithAccessToken ()
verseColored cal = mapM_ eventColored
  where
    newEV ev = ev { eventColorID = Just "11" } -- Tomato
    eventColored ev = replaceEvent cal (CeeEdible (newEV ev) ev)

insertEvent :: Calendar -> CalendarEvent -> WithAccessToken ()
insertEvent cal ev = do
  aToken <- asks fst
  runReq defaultHttpConfig $ do
    res  <- req POST (url cal) (ReqBodyJson ev) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: Value)
    liftIO $ putStrLn $ eventSummary ev ++ " registered!"

replaceEvent :: Calendar -> CalendarEventEqual -> WithAccessToken ()
replaceEvent cal (CeeEdible org gcal) = do
  aToken <- asks fst
  let evid = convertString $ eventID gcal
  let url' = url cal /: evid
  runReq defaultHttpConfig $ do
    res  <- req PUT url' (ReqBodyJson org) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: CalendarEvent)
    liftIO $ putStrLn $ eventSummary org ++ " replace!"
replaceEvent _ _ = return ()

searchCalendar :: Calendar -> IO ()
searchCalendar cal = do
  Encoding.setLocaleEncoding Encoding.utf8
  apair <- accessTokenPair
  (`runReaderT` apair) $ do
    gcalList <- getGoogleCalendarList cal
    oFile    <- liftIO orgFile
    events   <- nodeToCalendarEvents <$> liftIO (orgFileNode oFile)
    -- let diffs    = diffCalendarEvent events gcalList
    -- forM_ (filter isEdible diffs) replaceEvent
    -- forM_ (filter isCeeNot diffs) $ \(CeeNot c) -> insertEvent c
    let q = [ eventStart <+> QDate (fromGregorian 2025 5 7) ]
    forM_ (filter (matchQuery q) gcalList) $ liftIO . print . ColEV
    -- forM_ (filter (matchQuery q) events)   $ liftIO . print

getColors :: IO ()
getColors = do
  (aToken, _) <- accessTokenPair
  let url = https "www.googleapis.com"
            /: "calendar"
            /: "v3"
            /: "colors"
  runReq defaultHttpConfig $ do
    res  <- req GET url NoReqBody jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: GCC.ColorSet)
    -- liftIO $ putStrLn $ eventSummary org ++ " replace!"
-- GET https://www.googleapis.com/calendar/v3/colors


testInsert = do
  apair <- accessTokenPair
  insertEvent googleFamilyCalendar testEvent `runReaderT` apair
