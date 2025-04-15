{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
module Org.GoogleCalendar.Event
  (
    CalendarEvent (..)
  , AlmostEqual (..)
  , EdibleEqual (..)
  , ColorEvent (..)
  , eventDefault
  , testEvent
  , Q (..)
  , ComposeString (..)
  , ComposeDate (..)
  , isPersonal
  , matchQuery
  , matchQueryOr
  )
where

import  Data.Maybe              (fromMaybe, fromJust, isNothing, isJust)
import  Data.Either             (fromRight, isLeft)
import  Data.Function           (on)
import  Data.Aeson
import  Data.Aeson.Key          (fromString)
import  Data.Aeson.Types hiding (parse)
import  Data.Kind               (Type)
import  Data.Time
import  Data.List               (inits, tails)
import  Text.Parsec
import  Data.Functor.Const
import  qualified Data.Text     as Tx

data CalendarEvent =
  CalendarEvent { eventCreated     :: Either ParseError UTCTime
                , eventDescription :: Maybe Tx.Text
                , eventEnd         :: Maybe UTCTime
                , eventEtag        :: Tx.Text
                , eventIcalUID     :: Tx.Text
                , eventID          :: Tx.Text
                , eventStart       :: Maybe UTCTime
                , eventSummary     :: Tx.Text
                , eventUpdated     :: Either ParseError UTCTime
                , eventLocation    :: Maybe Tx.Text
                , eventColorID     :: Maybe Tx.Text
                , eventBirthDay    :: Maybe Value }
  deriving (Eq)

newtype EdibleEqual = Edible { runEdible :: CalendarEvent }
  deriving (Show)
newtype AlmostEqual = Almost { runAlmost :: CalendarEvent }
  deriving (Show)
newtype ColorEvent = ColEV { runColEV :: CalendarEvent }

instance Eq EdibleEqual where
  Edible (CalendarEvent _ _ _ _ _ _ st sumry _ _ _ _) ==
    Edible (CalendarEvent _ _ _ _ _ _ st' sumry' _ _ _ _)
    = (sumry == sumry')
      && ((utctDay <$> st) == (utctDay <$> st'))

instance Eq AlmostEqual where
  Almost (CalendarEvent _ dsc en _ _ _ st sumry _ loc _ _) ==
    Almost (CalendarEvent _ dsc' en' _ _ _ st' sumry' _ loc' _ _)
    = (dsc == dsc')
      && (en == en')
      && (st == st')
      && (sumry == sumry')
      && (loc == loc')

instance Show ColorEvent where
  show (ColEV e) = Tx.unpack str
    where
      str = eventSummary e
            <> " Color: "
            <> mempty `fromMaybe` eventColorID e

eventDefault :: CalendarEvent
eventDefault =
  CalendarEvent { eventCreated     = Right (UTCTime (fromGregorian 2025 4 1)
                                                    (secondsToDiffTime 0))
                , eventDescription = Nothing
                , eventEnd         = Nothing
                , eventEtag        = mempty
                , eventIcalUID     = mempty
                , eventID          = mempty
                , eventStart       = Nothing
                , eventSummary     = mempty
                , eventUpdated     = Right (UTCTime (fromGregorian 2025 4 1)
                                                    (secondsToDiffTime 0))
                , eventLocation    = Nothing
                , eventColorID     = Nothing
                , eventBirthDay    = Nothing }

testEvent :: CalendarEvent
testEvent =
  eventDefault { eventDescription = Just "<a href=\"http://foo\">hoge</a>"

               , eventEnd = Just $ UTCTime (fromGregorian 2025 4 1) (secondsToDiffTime 0)
               , eventEtag = "test etag"
               , eventIcalUID = "testUID"
               , eventID = "testID"
               , eventStart = Just $ UTCTime (fromGregorian 2025 3 31) (secondsToDiffTime 0)
               , eventSummary = "test summary"
               , eventLocation = Just "京建労会館"
               , eventColorID  = Just "11" }

instance FromJSON CalendarEvent where
  parseJSON (Object v) =
    let
      descRefine = (v .:? "description")
                   >>= return . (Tx.dropWhile (== '\n') <$>)
      startParse = toUTCTime <$> (v .: "start") -- Parse (Maybe UTCTime)
      endParse   = toUTCTime <$> (v .: "end")   -- Parse (Maybe UTCTime)
      endTime    = repairTime <$> startParse <*> endParse
      oneDayS    = 24 * 60 * 60
      repairTime stMaybe enMaybe =
        let
          predicates = [(/= stMaybe), (> (addUTCTime oneDayS <$> stMaybe))]
        in
          case all ($ enMaybe) predicates of
            True  -> UTCTime <$> (pred . utctDay <$> enMaybe)
                             <*> (utctDayTime <$> enMaybe)
            False -> enMaybe
    in
    CalendarEvent <$> (parse dateGreenWichParse "" <$> (v .: "created"))
                  <*> descRefine
                  <*> endTime
                  <*> (v .: "etag")
                  <*> (v .: "iCalUID")
                  <*> (v .: "id")
                  <*> startParse
                  <*> (v .: "summary")
                  <*> (parse dateGreenWichParse "" <$> (v .: "updated"))
                  <*> (v .:? "location")
                  <*> (v .:? "colorId")
                  <*> (v .:? "birthdayProperties")
  parseJSON invalid    =
    prependFailure "parsing CalendarEvent failed, "
    (typeMismatch "Object" invalid)

instance ToJSON CalendarEvent where
  toJSON (CalendarEvent _ dsc en etag _ _ st smry _ loc color _) =
    case timeObject <$> st <*> en of
      Nothing -> error "CalendarEvent ToJSON instance error"
      Just (st', en') ->
        let
          colorBox
            | isJust color = [ "colorId" .= fromJust color ]
            | otherwise    = mempty
          mainObj = [ "etag"        .= etag
                    , "summary"     .= smry
                    , "description" .= dsc
                    , "location"    .= mempty `fromMaybe` loc
                    , "start"       .= st'
                    , "end"         .= en']
        in
        object (mainObj <> colorBox)

instance Show CalendarEvent where
  show (CalendarEvent _ d e _ _ _ s summary _ _ _ _) =
    let
      st = mempty `fromMaybe` (Tx.pack . show <$> s)
      en = mempty `fromMaybe` (Tx.pack . show <$> e)
    in
      Tx.unpack $ Tx.concat [ st , "->" , en, " : "
                            , summary, ":", mempty `fromMaybe` d]
    -- let
    --   st = mempty `fromMaybe` (show <$> s)
    --   en = mempty `fromMaybe` (show <$> e)
    -- in
    --   st
    --   ++ "->"
    --   ++ en
    --   ++ " : "
    --   ++ summary
    --   ++ ":"
    --   ++ mempty `fromMaybe` d

instance Ord CalendarEvent where
  (CalendarEvent _ _ _ _ _ _ s1 _ _ _ _ _) `compare`
    (CalendarEvent _ _ _ _ _ _ s2 _ _ _ _ _) = s1 `compare` s2

data EventTime = EventTime (Maybe String) (Maybe String) deriving (Show, Eq)

instance FromJSON EventTime where
  parseJSON (Object v) = EventTime <$> (v .:? "date")
                                   <*> (v .:? "dateTime")
  parseJSON invalid    =
    prependFailure "parsing EventTime failed, "
    (typeMismatch "Object" invalid)

dateParse :: Parsec String () UTCTime
dateParse = do
  [y, m, d]  <- map read <$> sequence [ count 4 digit <* string "-"
                                      , count 2 digit <* string "-"
                                      , count 2 digit]
  [h, mi, s] <- (string "T" *> timeParse) <|> return [0, 0, 0]
  let day = fromGregorian (toInteger y) m d
  return $ UTCTime day (secondsToDiffTime (h * 3600 + mi * 60 + s))
  where
    timeParse = map read <$> sequence [ count 2 digit <* string ":"
                                      , count 2 digit <* string ":"
                                      , count 2 digit]

toJapaneseTime :: UTCTime -> UTCTime
toJapaneseTime = ((9 * 3600) `addUTCTime`)

dateGreenWichParse :: Parsec String () UTCTime
dateGreenWichParse = toJapaneseTime <$> dateParse

jpTimeLocale :: TimeLocale
jpTimeLocale =
  TimeLocale { wDays          =
               [ ("日曜日", "日") , ("月曜日", "月") , ("火曜日", "火")
               , ("水曜日", "水") , ("木曜日", "木")
               , ("金曜日", "金") , ("土曜日", "土")]
             , months         =
               [let m = show x ++ "月" in (m, m) | x <- [1..12] :: [Int]]
             , dateTimeFmt    = ""
             , dateFmt        = ""
             , timeFmt        = ""
             , time12Fmt      = ""
             , knownTimeZones = []
             , amPm           = (mempty, mempty)}

toUTCTime :: EventTime -> Maybe UTCTime
toUTCTime (EventTime d dt) =
  case parse dateParse "" <$> (d <> dt) of
    Nothing        -> Nothing
    Just (Right u) -> Just u
    _              -> Nothing

data GoogleTimeType = GDate | GDateTime deriving (Eq)

instance Show GoogleTimeType where
  show GDate = "date"
  show GDateTime = "dateTime"

googleTimeFormat :: UTCTime -> GoogleTimeType -> String
googleTimeFormat utc' GDate = formatTime jpTimeLocale "%Y-%m-%d" utc'
googleTimeFormat utc' GDateTime =
  formatTime jpTimeLocale "%Y-%m-%dT%H:%M:%S" utc'

makeTimeObject :: GoogleTimeType -> UTCTime -> Value
makeTimeObject gtype utc' = object (obj1 <> obj2)
  where
    obj1 = [ fromString(show gtype) .= googleTimeFormat utc' gtype ]
    obj2 = case gtype of
             GDate     -> mempty
             GDateTime -> [ "timeZone" .= ("Asia/Tokyo" :: String)]

doubleSwap :: (a -> b) -> (a, a) -> (b, b)
doubleSwap f = uncurry ((,) `on` f)

timeObject :: UTCTime -> UTCTime -> (Value, Value)
timeObject st en
  | utctDay st /= utctDay en =
    let en' = (24 * 60 * 60) `addUTCTime` en in
      makeTimeObject GDate `doubleSwap` (st, en')
  | utctDayTime st == 0 = makeTimeObject GDate `doubleSwap` (st, en)
  | otherwise = makeTimeObject GDateTime `doubleSwap` (st, en)

isPersonal :: CalendarEvent -> Bool
isPersonal ev = case eventDescription ev of
                  Just d  -> "Tasks/私用" `Tx.isPrefixOf` d
                  Nothing -> False

--------------------------------------------------
-- Query
--------------------------------------------------
type DateSet = (Integer, Int, Int)

data Q = QContains String
       | QRegexp String
       | QDate Day
       | QRange DateSet DateSet
       | QYearMonth (Integer, Int)
       | QYear Integer
       | QMonth Int
       | QDay Int
       | QDayOfWeek DayOfWeek

class Applicative f => ComposeDate (f :: Type -> Type) where
  unpureDate    :: f UTCTime -> UTCTime
  isFailureDate :: f UTCTime -> Bool
  runDateQ      :: (CalendarEvent -> f UTCTime) -> Q -> CalendarEvent -> Bool
  (<+>)         :: (CalendarEvent -> f UTCTime) -> Q ->
                          (CalendarEvent -> Bool)

  runDateQ f q event
    | isFailureDate (f event) = False
    | otherwise =
      let pureday      = utctDay $ unpureDate $ f event
          greg         = toGregorian pureday
          (y1, m1, d1) = greg
      in
        case q of
          QDate d           -> d == pureday
          QRange d1' d2'    -> (d1' <= greg) && (greg <= d2')
          QYear y           -> y == y1
          QMonth m          -> m == m1
          QDay d            -> d == d1
          QYearMonth (y, m) -> (y == y1) && (m == m1)
          QDayOfWeek dw     -> dw == dayOfWeek pureday
          _                 -> False

  f <+> q = runDateQ f q

class Applicative a => ComposeString (a :: Type -> Type) where
  unpureString     :: a String -> String
  isFailureString  :: a String -> Bool
  runStringQ       :: (CalendarEvent -> a String) -> Q ->
                         CalendarEvent -> Bool

  (<->) :: (CalendarEvent -> a String) -> Q -> (CalendarEvent -> Bool)

  runStringQ f q event
    | isFailureString (f event) = False
    | otherwise = case q of
                    QContains s -> s `contains` unpureString (f event)
                    _ -> False

  f <-> q = runStringQ f q

instance ComposeDate Maybe where
  unpureDate = fromJust
  isFailureDate = isNothing

instance ComposeDate (Either ParseError) where
  unpureDate = fromRight (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0))
  isFailureDate = isLeft

instance ComposeString (Const String) where
  unpureString    = getConst
  isFailureString = (== mempty) . unpureString

instance ComposeString Maybe where
  unpureString    = fromJust
  isFailureString = isNothing

contains :: String -> String -> Bool
contains [] _ = False
contains part subs = part `elem` (inits subs ++ tails subs)

matchQuery :: [CalendarEvent -> Bool] -> CalendarEvent -> Bool
matchQuery qs event = all ($ event) qs

matchQueryOr :: [CalendarEvent -> Bool] -> CalendarEvent -> Bool
matchQueryOr qs event = any ($ event) qs

-- test :: [CalendarEvent -> Bool]
-- test = [ (Const . eventSummary) <-> QContains "test"
--        , eventStart <+> QDate (fromGregorian 2025 4 1)]

-- class (Applicative f) => Compose f a where
--   unpure    :: f a -> a
--   isFailure :: f a -> Bool
--   predicate :: Q -> a -> Bool
--   -- runQ      :: (CalendarEvent -> f a) -> Q -> CalendarEvent -> Bool
--   runQ :: Q -> f a -> Bool

--   q `runQ` comp
--     | isFailure comp = False
--     | otherwise = q `predicate` unpure comp

-- instance Compose (Const String) String where
--   unpure = getConst
--   isFailure = (== mempty) . unpure
--   runQ  = runStringQ

-- instance Compose Maybe String where
--   unpure = fromJust
--   isFailure = isNothing
--   runQ = runStringQ

-- instance Compose Maybe UTCTime where
--   unpure = fromJust
--   isFailure = isNothing
--   runQ = runDateQ

-- instance Compose (Either ParseError) UTCTime where
--   unpure = fromRight (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0))
--   isFailure = isLeft
--   runQ = runDateQ

--   -- runQ getter q event
--   --   | isFailure (getter event) = False
--   --   | otherwise = q `predicate` unpure (getter event)
