module Org.ICS
  (
    VEVENT (..)
  , veventList
  )
where

import  Data.Time
import  Data.Time.Format
import  Org.Parse
import  Org.Node

jpTimeLocale :: TimeLocale
jpTimeLocale =
  TimeLocale { wDays = [ ("日曜日", "日") , ("月曜日", "月") , ("火曜日", "火")
                       , ("水曜日", "水") , ("木曜日", "木")
                       , ("金曜日", "金") , ("土曜日", "土")]
             , months = [let m = (show x) ++ "月" in (m, m) | x <- [1..12]]
             , dateTimeFmt = ""
             , dateFmt = ""
             , timeFmt = ""
             , time12Fmt = ""
             , knownTimeZones = []}

data VEVENT = VEVENT { vstarttime :: UTCTime
                     , vendtime   :: Maybe UTCTime
                     , vtitle     :: String
                     , vlocation  :: Maybe String
                     , vdesc      :: String
                     }
  deriving (Show, Eq)

utcToICS :: UTCTime -> String
utcToICS = formatTime jpTimeLocale "%Y%m%dT%H%M%S"

orgNodetoVEVENT :: String -> OrgElement -> OrgNode -> VEVENT
orgNodetoVEVENT path timestamp node =
  VEVENT { vstarttime = begin timestamp
         , vendtime   = end timestamp
         , vtitle     = node ~~> title
         , vlocation  = location node
         , vdesc      = path <> "\n" <> description node
         }

veventList :: OrgNode -> [VEVENT]
veventList node =
  concatMap (\(path, n) ->
               map (\time -> orgNodetoVEVENT path time n) (timestamps n)) collecter
  where
    collecter = collectNodeList filf (Just node)
    filf n    = (hasAliveTime n) && (notTODO n)
