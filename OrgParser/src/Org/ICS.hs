{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
module Org.ICS
  (
    -- VEVENT (..)
  -- , VCalendar (..)
  -- , veventList
  -- , nodeToVCalendar
  -- , vcalendarToString
    getGoogleCalendarList
  , updateGoogleCalendar
  , insertEvent
  )
where

import  Data.Maybe              (fromMaybe)
import  Data.List               (intercalate)
-- import  Data.Monoid
import  Data.Text               (Text (..))
import  Data.Aeson
-- import  Data.Aeson.Types
import  Data.Time
-- import  Data.Time.Format
import  Data.String.Conversions
import  qualified Data.Map.Strict as M
import  Control.Monad.Reader
-- import  Control.Monad.IO.Class (liftIO)
import  Network.HTTP.Req
import  Org.Parse
import  Org.Node
import  Org.GoogleCalendar.Client
import  Org.GoogleCalendar.Event
import  System.Directory
import  Text.Hamlet
import  Text.Blaze.Html.Renderer.String
import  qualified Data.ByteString.Lazy as B
-- import  qualified Data.ByteString.Lazy.Internal as BI


-- data VEVENT = VEVENT { vstarttime :: UTCTime
--                      , vendtime   :: Maybe UTCTime
--                      , vtitle     :: String
--                      , vlocation  :: Maybe String
--                      , vdesc      :: String
--                      }
--   deriving (Eq)

-- data VCalendar = VCalendar [VEVENT]
--   deriving (Show, Eq)

-- testV = VEVENT (mktime 2025 3 18 12 0) Nothing "" Nothing ""

-- instance Show VEVENT where
--   show v =
--     renderHtml [shamlet|
--                        BEGIN:VEVENT
--                        DTSTART:#{vs}
--                        #{dtend}
--                        LOCATION:#{vl}
--                        DESCRIPTION:#{vd}
--                        SUMMARY:#{vt}
--                        END:VEVENT
--                        |]
--     where
--       vs = utcToICS $ vstarttime v
--       vl = mempty `fromMaybe` vlocation v
--       vd = vdesc v
--       vt = vtitle v
--       dtend = mempty `fromMaybe` ((("DTEND:" ++) . utcToICS) <$> vendtime v)

-- utcToICS :: UTCTime -> String
-- utcToICS utc
--   | utctDayTime utc == 0 = formatTime jpTimeLocale "%Y%m%d" utc
--   | otherwise = formatTime jpTimeLocale "%Y%m%dT%H%M%S" utc

-- titleToVEVENT :: OrgTitle -> [VEVENT]
-- titleToVEVENT a = map (makeVEVENT a) $ otimestamps a
--   where
--     makeVEVENT ttl time' =
--       VEVENT { vstarttime = obegin time'
--              , vendtime   = oend time'
--              , vtitle     = otitle ttl
--              , vlocation  = Nothing
--              , vdesc      = "" }

-- veventList :: Node OrgTitle -> [VEVENT]
-- veventList node =
--   concatMap titleToVEVENT $ nodeCollectList normalFilter node

-- nodeToVCalendar :: Node OrgTitle -> VCalendar
-- nodeToVCalendar = VCalendar . veventList

-- vcalendarToString :: VCalendar -> String
-- vcalendarToString (VCalendar vevs) =
--   renderHtml [shamlet|
--                      BEGIN:VCALENDAR
--                      VERSION:2.0
--                      PRODID:-//hacksw/handcal//NONSGML v1.0//EN
--                      #{vevents}
--                      END:VCALENDAR
--                      |]
--   where
--     vevents = intercalate "\n" $ map show vevs

type Config = M.Map Text String


-- ["e:/Foo", "e:/Dropbox/access.json"]

-- jsonFile :: [FilePath] -> IO (Maybe FilePath)
-- jsonFile [] = return Nothing
-- jsonFile (x:xs) = do
--   fp <- doesPathExist x
--   case fp of
--     True  -> return (Just x)
--     False -> return Nothing

-- fromAccessJSON :: IO (Maybe Config)
-- fromAccessJSON = do
--   Just jsonfile   <- jsonFile [ "c:/Users/Jumpei/Documents/home/OrgFiles/access.json"
--                               , "e:/Dropbox/access.json"]
  --   bytestring <- B.readFile jsonfile
--   return $ decode bytestring
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

getGoogleCalendarList :: IO [CalendarEvent]
getGoogleCalendarList = loop Nothing []
  where
    loop pageToken ret = do
      Calendar events np <- getGoogleCalendarPage pageToken
      case np of
        Just _  -> loop np (ret ++ events)
        Nothing -> return (ret ++ events)

getGoogleCalendarPage :: Maybe String -> IO Calendar
getGoogleCalendarPage nextToken = do
  client <- clientFromFile "c:/Users/Jumpei/Documents/home/OrgFiles/access.json"
  runReq defaultHttpConfig $ do
    aToken <- liftIO (aliveAccessToken `runReaderT` client)
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

updateGoogleCalendar :: [CalendarEvent] -> IO ()
updateGoogleCalendar = mapM_ insertEvent

insertEvent :: CalendarEvent -> IO ()
insertEvent cal = do
  client <- clientFromFile "c:/Users/Jumpei/Documents/home/OrgFiles/access.json"
  runReq defaultHttpConfig $ do
    aToken <- liftIO (aliveAccessToken `runReaderT` client)
    _      <- req POST googleCalendarHttps (ReqBodyJson cal) ignoreResponse
                  (headerAuthorization aToken)
    liftIO $ putStrLn $ eventSummary cal ++ " registered!"

data Calendar = Calendar [CalendarEvent] (Maybe String) deriving (Show)

instance FromJSON Calendar where
  parseJSON (Object v) = Calendar <$> (v .: "items")
                                  <*> (v .:? "nextPageToken")
