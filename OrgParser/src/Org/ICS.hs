{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
module Org.ICS
  (
    getGoogleCalendarList
  , updateGoogleCalendar
  , googleCalendar
  , googleFamilyCalendar
  , searchCalendar
  , getColors
  , insertEvent
  )
where

import           Data.List                   (sort, elemIndex)
import           Data.Text                   (Text)
import           Data.Function               (on)
import           Data.Time
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe                  (fromJust, isJust)
import           Data.String.Conversions     (convertString)
import           Control.Monad               (forM_)
import           Control.Monad.State
import           Network.HTTP.Req
import           Org.Conduit                 (forICS)
import           Org.Google.Client           (App, Client (..)
                                             , appCoreCalendar)
import           Org.GoogleCalendar.Event
import qualified Org.GoogleCalendar.Color    as GCC
import qualified GHC.IO.Encoding             as Encoding
import qualified Data.Text.IO                as TxIO

data CalendarEventEqual = CeeAlmost CalendarEvent
                        | CeeEdible CalendarEvent CalendarEvent
                        | CeeNot CalendarEvent
                        deriving (Eq)

data CalendarResponse =
  CalendarResponse [CalendarEvent] (Maybe Text)
  deriving (Show)

data Calendar = Calendar { calendarID  :: Text
                         , url         :: Url 'Https
                         , filterEvent :: CalendarEvent -> Bool }

data CeeMatcher = CM { cmDesc :: Bool
                     , cmEndt :: Bool
                     , cmStat :: Bool
                     , cmSumm :: Bool
                     , cmLoca :: Bool }
  deriving (Show)

instance Show CalendarEventEqual where
  show (CeeAlmost c)     = "CeeAlmost " ++ show c
  show (CeeNot c)        = "CeeNot " ++ show c
  show (CeeEdible c1 c2) = "CeeEdible\n" ++ show c1 ++ "\n" ++ show c2 ++ "\n"

instance FromJSON CalendarResponse where
  parseJSON (Object v) =
    CalendarResponse <$> (v .: "items")
                     <*> (v .:? "nextPageToken")
  parseJSON invalid    =
    prependFailure "parsing Calendar failed, "
    (typeMismatch "Object" invalid)


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

headerAuthorization :: Text -> Option scheme
headerAuthorization atoken =
  header "Authorization" ("Bearer " <> convertString atoken)

-- accessTokenPair :: IO (String, Client)
-- accessTokenPair = do
--   c <- clientFromFile
--   a <- aliveAccessToken `runReaderT` c
--   return (a, c)

getGoogleCalendarList :: Calendar -> App [CalendarEvent]
getGoogleCalendarList cal = sort <$> loop Nothing []
  where
    loop pageToken ret = do
      CalendarResponse events np <- getGoogleCalendarPage pageToken cal
      case np of
        Just _  -> loop np (ret ++ events)
        Nothing -> return (ret ++ events)

getGoogleCalendarPage :: Maybe Text -> Calendar -> App CalendarResponse
getGoogleCalendarPage nextToken cal = do
  aToken <- accessToken . snd <$> get
  runReq defaultHttpConfig $ do
    res <- req GET (url cal) NoReqBody jsonResponse
               (headerAuthorization aToken <> query)
    return $ responseBody res
      where
        options :: [(Text, Text)]
        query   :: Option scheme
        pToken Nothing  = []
        pToken (Just k) = [ ("pageToken", k)]
        options         = [ ("maxResults", "100")
                          , ("singleEvents", "true")]
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
  -- apair <- accessTokenPair
  appCore <- appCoreCalendar
  (`evalStateT` appCore) $ do
    gcalList <- getGoogleCalendarList cal
    allev    <- liftIO forICS
    let events    = filter (filterEvent cal) allev
    let diffs     = diffCalendarEvent events gcalList
    let diffVerse = diffVerseCalendarEvent events gcalList
    -- forM_ (filter isEdible diffs) $ \(CeeEdible c1 c2) -> do
    --   liftIO $ print (CeeEdible c1 c2)
    --   liftIO $ print $ _makeCeeMatcher (CeeEdible c1 c2)
    --   liftIO $ print $ show $ eventDescription c1
    --   liftIO $ print $ show $ eventDescription c2
    edibleEventsReplace cal diffs
    newEventsInsert cal diffs
    verseColored cal diffVerse

edibleEventsReplace :: Calendar -> [CalendarEventEqual] -> App ()
edibleEventsReplace cal events =
  forM_ (filter isEdible events) $ replaceEvent cal

newEventsInsert :: Calendar -> [CalendarEventEqual] -> App ()
newEventsInsert cal events =
  forM_ (filter isCeeNot events) $ \case
     CeeNot s -> insertEvent cal s
     _        -> return ()

verseColored :: Calendar -> [CalendarEvent] -> App ()
verseColored cal = mapM_ eventColored
  where
    newEV ev = ev { eventColorID = Just "11" } -- Tomato
    eventColored ev
      | isJust (eventBirthDay ev) = return ()
      | otherwise = replaceEvent cal (CeeEdible (newEV ev) ev)

insertEvent :: Calendar -> CalendarEvent -> App ()
insertEvent cal ev = do
  aToken <- accessToken . snd <$> get
  runReq defaultHttpConfig $ do
    res  <- req POST (url cal) (ReqBodyJson ev) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: CalendarEvent)
    liftIO $ TxIO.putStrLn $ eventSummary ev <> " registered!"

replaceEvent :: Calendar -> CalendarEventEqual -> App ()
replaceEvent cal (CeeEdible org gcal) = do
  aToken <- accessToken . snd <$> get
  let evid = convertString $ eventID gcal
  let url' = url cal /: evid
  runReq defaultHttpConfig $ do
    res  <- req PUT url' (ReqBodyJson org) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: CalendarEvent)
    liftIO $ TxIO.putStrLn $ eventSummary org <> " replace!"
replaceEvent _ _ = return ()

searchCalendar :: Calendar -> IO ()
searchCalendar cal = do
  Encoding.setLocaleEncoding Encoding.utf8
  -- apair <- accessTokenPair
  appCore <- appCoreCalendar
  (`evalStateT` appCore) $ do
    gcalList <- getGoogleCalendarList cal
   -- events   <- liftIO forICS
    -- let diffs    = diffCalendarEvent events gcalList
    -- forM_ (filter isEdible diffs) replaceEvent
    -- forM_ (filter isCeeNot diffs) $ \(CeeNot c) -> insertEvent c
    let q = [ eventStart <+> QDate (fromGregorian 2025 5 7) ]
    forM_ (filter (matchQuery q) gcalList) $ liftIO . print . ColEV
    -- forM_ (filter (matchQuery q) events)   $ liftIO . print

getColors :: IO ()
getColors = do
  appCore <- appCoreCalendar
  let aToken = accessToken $ snd appCore
  let url' = https "www.googleapis.com"
             /: "calendar"
             /: "v3"
             /: "colors"
  runReq defaultHttpConfig $ do
    res  <- req GET url' NoReqBody jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: GCC.ColorSet)
    -- liftIO $ putStrLn $ eventSummary org ++ " replace!"
-- GET https://www.googleapis.com/calendar/v3/colors


---- for debug --------------------------------------------------
_isCeeAlmost :: CalendarEventEqual -> Bool
_isCeeAlmost (CeeAlmost _) = True
_isCeeAlmost _ = False

_makeCeeMatcher :: CalendarEventEqual -> CeeMatcher
_makeCeeMatcher (CeeEdible c1 c2) =
  let judge f = ((==) `on` f) c1 c2 in
  CM (judge eventDescription)
     (judge eventEnd)
     (judge eventStart)
     (judge eventSummary)
     (judge eventLocation)
_makeCeeMatcher _ = error ""

-- _testInsert :: IO ()
-- _testInsert = do
--   apair <- accessTokenPair
--   insertEvent googleFamilyCalendar testEvent `runStateT` apair
