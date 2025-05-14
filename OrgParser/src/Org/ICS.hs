{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Control.Concurrent.Async as A
import           Control.Monad            (forM_)
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function            (on)
import qualified Data.List                as Dl
import           Data.Maybe               (fromJust, isJust)
import qualified Data.Set                 as S
import           Data.String.Conversions  (convertString)
import           Data.Text                (Text)
import qualified Data.Text.IO             as TxIO
import           Data.Time
import qualified Data.Vector              as V
import qualified GHC.IO.Encoding          as Encoding
import           Network.HTTP.Req
import           Org.Conduit              (forICSVector)
import           Org.Google.Client        (App, Client (..), Config,
                                           appCoreCalendar)
import qualified Org.GoogleCalendar.Color as GCC
import           Org.GoogleCalendar.Event

data CalendarEventEqual = CeeAlmost CalendarEvent
                        | CeeEdible CalendarEvent CalendarEvent
                        | CeeNot CalendarEvent
                        deriving (Eq)

data CalendarResponse =
  CalendarResponse (V.Vector CalendarEvent) (Maybe Text)
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
isEdible _               = False

isCeeNot :: CalendarEventEqual -> Bool
isCeeNot (CeeNot _) = True
isCeeNot _          = False

headerAuthorization :: Text -> Option scheme
headerAuthorization atoken =
  header "Authorization" ("Bearer " <> convertString atoken)

getGoogleCalendarList :: Calendar -> App [CalendarEvent]
getGoogleCalendarList cal = Dl.sort <$> loop Nothing []
  where
    loop pageToken ret = do
      CalendarResponse events np <- getGoogleCalendarPage pageToken cal
      case np of
        Just _  -> loop np (ret ++ V.toList events)
        Nothing -> return (ret ++ V.toList events)

getGoogleCalendarVector :: Calendar -> App (V.Vector CalendarEvent)
getGoogleCalendarVector cal = loop Nothing V.empty
  where
    loop pageToken ret = do
      CalendarResponse events np <- getGoogleCalendarPage pageToken cal
      case np of
        Just _  -> loop np (V.concat [ret, events])
        Nothing -> return (V.concat [ret, events])

getGoogleCalendarPage :: Maybe Text -> Calendar -> App CalendarResponse
getGoogleCalendarPage nextToken cal = do
  aToken <- gets (accessToken . snd)
  runReq defaultHttpConfig $ do
    res <- req GET (url cal) NoReqBody jsonResponse
               (headerAuthorization aToken <> query)
    -- liftIO $ print (responseBody res)
    return (responseBody res :: CalendarResponse)
      where
        options :: [(Text, Text)]
        query   :: Option scheme
        pToken Nothing  = []
        pToken (Just k) = [ ("pageToken", k)]
        options         = [ ("maxResults", "100")
                          , ("singleEvents", "true")]
                             <> pToken nextToken
        query           = foldMap (uncurry (=:)) options

-- diffCalendarEvent ::
--   [CalendarEvent] ->    -- Org Events
--   [CalendarEvent] ->    -- Google Calendar Events
--   [CalendarEventEqual]  -- Org Events that Gcal doesn't have
diffCalendarEvent ::
  V.Vector CalendarEvent   ->
  S.Set AlmostEqual        ->
  S.Set EdibleEqual        ->
  V.Vector CalendarEvent   ->
  V.Vector CalendarEventEqual
diffCalendarEvent orgEv almostSet edibleSet gcalEV = V.map judge orgEv
  where
    judge ev
      | Almost ev `S.member` almostSet = CeeAlmost ev
      | Edible ev `S.member` edibleSet =
        let k = ev `V.elemIndex` gcalEV in
          CeeEdible ev (gcalEV V.! fromJust k)
      | otherwise = CeeNot ev
    -- almost = V.map Almost gcalEv
    -- edible = V.map Edible gcalEv
    -- judge ev
    --   | Almost ev `V.elem` almost = CeeAlmost ev
    --   | Edible ev `V.elem` edible =
    --       let
    --         k = Edible ev `V.elemIndex` edible
    --       in
    --         CeeEdible ev (gcalEv V.! fromJust k)
    --   | otherwise = CeeNot ev

-- diffVerseCalendarEvent ::
--   [CalendarEvent] -> -- Org Events
--   [CalendarEvent] -> -- Google Calendar Events
--   [CalendarEvent]    -- Google events that Org doesn't have
diffVerseCalendarEvent ::
  S.Set AlmostEqual ->
  V.Vector CalendarEvent ->
  V.Vector CalendarEvent
diffVerseCalendarEvent orgSet = V.filter judge
  where
    judge ev
      | Almost ev `S.member` orgSet = False
      | eventColorID ev == Just "11" = False -- Already Colored Events excepts
      | otherwise = True

runAppCalendar :: StateT (Config, Client) IO b -> IO b
runAppCalendar f = do
  app <- appCoreCalendar
  f `evalStateT` app

updateGoogleCalendar :: Calendar -> IO ()
updateGoogleCalendar cal = do
  Encoding.setLocaleEncoding Encoding.utf8
  -- appCore <- appCoreCalendar
  -- A.withAsync (getGoogleCalendarVector cal `evalStateT` appCore) $ \gl -> do
  A.withAsync (runAppCalendar $ getGoogleCalendarVector cal) $ \gl -> do
    A.withAsync forICSVector $ \al-> do
      gcalList <- A.wait gl
      allev    <- A.wait al
      let events    = V.filter (filterEvent cal) allev
      let setMap f  = S.fromList . V.toList . V.map f
      let almostSet = setMap Almost gcalList
      let edibleSet = setMap Edible gcalList
      let almostOrg = setMap Almost events
      let diffs     = diffCalendarEvent events almostSet edibleSet gcalList
      let diffVerse = diffVerseCalendarEvent almostOrg gcalList
      A.mapConcurrently_ runAppCalendar
                         [ edibleEventsReplace diffs cal
                         , newEventsInsert diffs cal
                         , verseColored diffVerse cal]

edibleEventsReplace :: V.Vector CalendarEventEqual -> Calendar -> App ()
edibleEventsReplace events cal =
  forM_ (V.filter isEdible events) $ replaceEvent cal

newEventsInsert :: V.Vector CalendarEventEqual -> Calendar -> App ()
newEventsInsert events cal =
  forM_ (V.filter isCeeNot events) $ \case
     CeeNot s -> insertEvent cal s
     _        -> return ()

verseColored :: V.Vector CalendarEvent -> Calendar -> App ()
verseColored events cal = mapM_ eventColored events
  where
    newEV ev = ev { eventColorID = Just "11" } -- Tomato
    eventColored ev
      | isJust (eventBirthDay ev) = return ()
      | otherwise = replaceEvent cal (CeeEdible (newEV ev) ev)

insertEvent :: Calendar -> CalendarEvent -> App ()
insertEvent cal ev = do
  aToken <- gets (accessToken . snd)
  runReq defaultHttpConfig $ do
    res  <- req POST (url cal) (ReqBodyJson ev) jsonResponse
              (headerAuthorization aToken)
    liftIO $ print (responseBody res :: CalendarEvent)
    liftIO $ TxIO.putStrLn $ eventSummary ev <> " registered!"

replaceEvent :: Calendar -> CalendarEventEqual -> App ()
replaceEvent cal (CeeEdible org gcal) = do
  aToken <- gets (accessToken . snd)
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
_isCeeAlmost _             = False

_makeCeeMatcher :: CalendarEventEqual -> CeeMatcher
_makeCeeMatcher (CeeEdible c1 c2) =
  let judge f = ((==) `on` f) c1 c2 in
  CM (judge eventDescription)
     (judge eventEnd)
     (judge eventStart)
     (judge eventSummary)
     (judge eventLocation)
_makeCeeMatcher _ = error ""
