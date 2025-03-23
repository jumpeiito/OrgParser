{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
module Org.ICS
  (
    VEVENT (..)
  , VCalendar (..)
  , veventList
  , nodeToVCalendar
  , vcalendarToString
  )
where

import  Data.Maybe              (fromMaybe)
import  Data.List               (intercalate)
import  Data.Time
import  Data.Time.Format
import  Network.HTTP.Req
import  Org.Parse
import  Org.Node
import  Text.Hamlet
import  Text.Blaze.Html.Renderer.String

jpTimeLocale :: TimeLocale
jpTimeLocale =
  TimeLocale { wDays          = [ ("日曜日", "日") , ("月曜日", "月") , ("火曜日", "火")
                                , ("水曜日", "水") , ("木曜日", "木")
                                , ("金曜日", "金") , ("土曜日", "土")]
             , months         = [let m = (show x) ++ "月" in (m, m) | x <- [1..12]]
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

orgNodetoVEVENT :: String -> OrgElement -> OrgNode -> VEVENT
orgNodetoVEVENT path timestamp node =
  VEVENT { vstarttime = begin timestamp
         , vendtime   = end timestamp
         , vtitle     = node ~~> title
         , vlocation  = location node
         , vdesc      = path <> description node
         }

veventList :: OrgNode -> [VEVENT]
veventList node =
  concatMap (\(path, n) ->
               map (\time -> orgNodetoVEVENT path time n) (timestamps n)) collecter
  where
    collecter = collectNodeList filf (Just node)
    filf n    = (hasAliveTime n) && (notTODO n)

nodeToVCalendar :: OrgNode -> VCalendar
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
      NoReqBody
      ignoreResponse
      mempty
    return ()
    -- liftIO $ print (responseBody res :: Value)
      where
        url :: Url 'Https
        url = https "googleapis.com" /: "calendar" /: "v3" /: "calendars" /: "junnpit@gmail.com" /: "events" /: "import"
        -- https://www.googleapis.com/calendar/v3/calendars/calendarId/events/import
        -- params :: FormUrlEncodedParam
        -- params =    "key1" =: ("value1" :: Text)
        --             <> "key2" =: (2 :: Int)
