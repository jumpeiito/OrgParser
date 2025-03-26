{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
module Org.ICS
  (
    VEVENT (..)
  , VCalendar (..)
  -- , veventList
  -- , nodeToVCalendar
  -- , vcalendarToString
  )
where

import  Data.Maybe              (fromMaybe)
import  Data.List               (intercalate)
import  Data.Time
import  Data.Time.Format
import  Data.Aeson
import  Data.Aeson.Types
import  qualified Data.Map.Strict as M
import  Control.Monad.IO.Class (liftIO)
import  Network.HTTP.Req
import  Org.Parse
import  Org.Node
import  Text.Hamlet
import  Text.Blaze.Html.Renderer.String

jpTimeLocale :: TimeLocale
jpTimeLocale =
  TimeLocale { wDays          =
               [ ("日曜日", "日") , ("月曜日", "月") , ("火曜日", "火")
               , ("水曜日", "水") , ("木曜日", "木")
               , ("金曜日", "金") , ("土曜日", "土")]
             , months         =
               [let m = (show x) ++ "月" in (m, m) | x <- [1..12]]
             , dateTimeFmt    = ""
             , dateFmt        = ""
             , timeFmt        = ""
             , time12Fmt      = ""
             , knownTimeZones = []}

data VEVENT = VEVENT { vstarttime :: UTCTime
                     , vendtime   :: Maybe UTCTime
                     , vtitle     :: String
                     , vlocation  :: Maybe String
                     , vdesc      :: String
                     }
  deriving (Eq)

data VCalendar = VCalendar [VEVENT]
  deriving (Show, Eq)

testV = VEVENT (mktime 2025 3 18 12 0) Nothing "" Nothing ""

instance Show VEVENT where
  show v =
    renderHtml [shamlet|
                       BEGIN:VEVENT
                       DTSTART:#{vs}
                       #{dtend}
                       LOCATION:#{vl}
                       DESCRIPTION:#{vd}
                       SUMMARY:#{vt}
                       END:VEVENT
                       |]
    where
      vs = utcToICS $ vstarttime v
      vl = mempty `fromMaybe` vlocation v
      vd = vdesc v
      vt = vtitle v
      dtend = mempty `fromMaybe` ((("DTEND:" ++) . utcToICS) <$> vendtime v)

utcToICS :: UTCTime -> String
utcToICS utc
  | utctDayTime utc == 0 = formatTime jpTimeLocale "%Y%m%d" utc
  | otherwise = formatTime jpTimeLocale "%Y%m%dT%H%M%S" utc

titleToVEVENT :: OrgTitle -> [VEVENT]
titleToVEVENT a = map (makeVEVENT a) $ otimestamps a
  where
    makeVEVENT ttl time' =
      VEVENT { vstarttime = obegin time'
             , vendtime   = oend time'
             , vtitle     = otitle ttl
             , vlocation  = Nothing
             , vdesc      = "" }

veventList :: Node OrgTitle -> [VEVENT]
veventList node =
  concatMap titleToVEVENT $ nodeCollectList normalFilter node

nodeToVCalendar :: Node OrgTitle -> VCalendar
nodeToVCalendar = VCalendar . veventList

vcalendarToString :: VCalendar -> String
vcalendarToString (VCalendar vevs) =
  renderHtml [shamlet|
                     BEGIN:VCALENDAR
                     VERSION:2.0
                     PRODID:-//hacksw/handcal//NONSGML v1.0//EN
                     #{vevents}
                     END:VCALENDAR
                     |]
  where
    vevents = intercalate "\n" $ map show vevs

postSample :: IO ()
postSample = do
  runReq defaultHttpConfig $ do
    res <- req
      POST
      url
      (ReqBodyJson body)
      jsonResponse
      mempty
    liftIO $ print (responseBody res :: Value)
      where
        url :: Url 'Https
        url = https "www.googleapis.com" /: "calendar" /: "v3" /: "calendars" /: "primary" /: "events"
        -- https://www.googleapis.com/calendar/v3/calendars/primary/events
        body = toJSON $ M.fromList ([("key", "AIzaSyAe-ptJR-3mvcAEfK3EJh5_pyidHEGE7G8")] :: [(String, String)])
