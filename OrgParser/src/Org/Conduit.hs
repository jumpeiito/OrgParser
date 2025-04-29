{-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Org.Conduit
  (
    forICS
  , forICSVector
  , forGeocode
  , documentSource
  , documentProducer
  , normalConduit
  , documentConduit
  , GeocodeMap
  )
where

import           Prelude                    hiding (takeWhile)
import           Control.Applicative        ((<|>))
import           Control.Monad.State
import           Text.Megaparsec            (parse)
import           Data.Maybe                 (fromJust)
import qualified Data.Map.Strict            as Map
import           Data.Conduit
import           Data.Conduit.List          (sourceList, consume)
import qualified Data.Conduit.List          as CL
import qualified Data.Conduit.Combinators   as CC
import qualified Data.List                  as Dl
import qualified Data.Text                  as Tx
import qualified Data.Text.IO               as TxIO
import qualified Data.Vector                as V
import           Control.Lens               hiding ((:>), noneOf)
import           System.Environment         (getEnv)
import           System.Directory           (getDirectoryContents)
import qualified GHC.IO.Encoding            as Encoding
-- import           Org.ParseText
import           Org.Parse.Time
import           Org.Parse.Text
import           Org.Node                   (Node (..), Nodeable (..))
import           Org.GoogleCalendar.Event   (CalendarEvent (..)
                                            , eventDefault)
------------------------------------------------------------
type GeocodeMap text = Map.Map Tx.Text [Title text]

data SelectType text = SelectTrue (Title text) | SelectFalse (Title text)
  deriving (Show, Eq)

isSelectTrue :: SelectType text -> Bool
isSelectTrue (SelectTrue _) = True
isSelectTrue _ = False

runSelectType :: SelectType text -> (Title text)
runSelectType (SelectTrue s) = s
runSelectType (SelectFalse s) = s

fileLines :: FilePath -> IO [Tx.Text]
fileLines fp = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  contents <- liftIO $ TxIO.readFile fp
  return $ Tx.lines contents

orgFile :: IO FilePath
orgFile = flip (++) "/notes.org" <$> getEnv "ORG"

orgArchiveFiles :: IO [FilePath]
orgArchiveFiles = do
  orgDir   <- getEnv "ORG"
  contents <- getDirectoryContents orgDir
  let fullpaths = map (\n -> orgDir ++ "\\" ++ n) contents
  return $ filter ("org_archive" `Dl.isSuffixOf`) fullpaths

noteSource :: ConduitT () Tx.Text IO ()
noteSource = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  filepath <- liftIO orgFile
  lines'   <- liftIO $ fileLines filepath
  mapM_ yield lines'

archiveSource :: ConduitT () Tx.Text IO ()
archiveSource = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  files    <- liftIO orgArchiveFiles
  contents <- liftIO $ mapM fileLines files
  mapM_ yield (foldMap (<>) (drop 6 contents) [])

orgSource :: ConduitT () Tx.Text IO ()
orgSource = noteSource <> archiveSource

documentSource :: FilePath -> ConduitT () Tx.Text IO ()
documentSource document = liftIO (fileLines document)
                          >>= mapM_ yield

geocodeProducer :: TitleBuilder text =>
  ((Title text) -> Bool) -> ConduitT () (SelectType text) IO ()
geocodeProducer f = noteSource
                    .| titleConduit
                    .| selectConduit f
                    .| selectTitleConduit
------------------------------------------------------------
titleConduit ::
  TitleBuilder text => ConduitT Tx.Text (Title text) IO ()
titleConduit = do
  lastTitle <- loop Nothing
  case lastTitle of
    Just lt -> yield lt
    Nothing -> return ()
  where
    loop :: TitleBuilder text =>
            Maybe (Title text) ->
            ConduitT Tx.Text (Title text) IO (Maybe (Title text))
    loop current = do
      stream <- await
      case stream of
        Nothing -> return current
        Just txt ->
          case (current, parse lineParse "" txt) of
            (_, Left _)             -> loop current
            (Nothing, Right (LL t)) -> loop (Just t)
            (Nothing, _)            -> loop current
            (Just c, Right (LL t))  -> do { yield c; loop (Just t) }
            (Just c, Right LB) ->
              loop $ Just (c & #paragraph %~ (<> fromToken "\\r\\n\n"))
            (Just c, Right (LP ("LOCATION", l))) -> do
              loop $ Just (c & #location .~ (l, mempty))
            (Just c, Right (LP ("PROPERTIES", _))) -> do
              loop $ Just c
            (Just c, Right (LP ("END", _))) -> do
              loop $ Just c
            (Just c, Right (LP ps)) -> do
              loop $ Just (c & #properties %~ (<> [ps]))
            (Just c, Right (LO o)) -> do
              loop $ Just (o `mplusOther` c)

nodeConduitGenerator :: (Node (Title a) -> b) -> ConduitT (Title a) b IO ()
nodeConduitGenerator nodeModify = do
  nodeTree <- loop None
  yield (nodeModify nodeTree)
  where
    loop current = do
      stream <- await
      case stream of
        Nothing    -> return current
        Just title -> loop $ build title current

nodeConduit :: ConduitT (Title a) (Node (Title a)) IO ()
nodeConduit = nodeConduitGenerator (cut f)
  where
    f ttl = (ttl ^. #label == "プログラムメモ") && (ttl ^. #level == 2)

-- _pickConduit :: TitleBuilder text =>
--   (Title -> Bool) -> ConduitT (Title text) (Node (Title text)) IO ()
-- _pickConduit f = nodeConduitGenerator (pick f)

selectConduit :: ((Title a) -> Bool)
  -> ConduitT (Title a) (Node (Title a), Node (Title a)) IO ()
selectConduit f = nodeConduitGenerator (select f)

titleBackConduit :: ConduitT (Node (Title a)) (Title a) IO ()
titleBackConduit = do
  node <- await
  case node of
    Just n  -> sourceList $ scrap n
    Nothing -> return ()

titleBackAllConduit :: ConduitT (Node (Title a)) (Title a) IO ()
titleBackAllConduit = do
  node <- await
  case node of
    Just n  -> sourceList $ scrapAll n
    Nothing -> return ()

documentConduit :: TitleBuilder text => ConduitT Tx.Text (Title text) IO ()
documentConduit = do
  titleConduit
  .| nodeConduit
  .| titleBackAllConduit

documentProducer :: TitleBuilder text => FilePath -> ConduitT () (Title text) IO ()
documentProducer fp = documentSource fp .| documentConduit

normalConduit :: TitleBuilder text => ConduitT Tx.Text (Title text) IO ()
normalConduit = do
  titleConduit
    .| nodeConduit
    .| titleBackConduit

selectTitleConduit ::
  ConduitT (Node (Title a), Node (Title a)) (SelectType a) IO ()
selectTitleConduit = do
  s <- await
  case s of
    Nothing -> return ()
    Just (strue, sfalse) -> do
      let toSource f = sourceList . map f . scrap
      toSource SelectTrue strue <> toSource SelectFalse sfalse
------------------------------------------------------------
eventSink :: TitleBuilder text => ConduitT (Title text) Void IO [CalendarEvent]
eventSink = do
  titles <- consume
  let makeEvent title = map (`toEvent` title) $ title ^. #timestamps
  return $ concatMap makeEvent titles

eventSinkVector :: TitleBuilder text =>
  ConduitT (Title text) Void IO (V.Vector CalendarEvent)
eventSinkVector = do
  titles <- CC.sinkVector
  let timestampsV title = V.fromList $ title ^. #timestamps
  let makeEvent title = V.map (`toEvent` title) $ timestampsV title
  return $ V.concatMap makeEvent titles

locationSink :: ConduitT (Title a) Void IO (GeocodeMap a)
locationSink = CL.fold mapInsert Map.empty
  where
    mapInsert m t =
      let
        (loc, ges) = t ^. #location
        ges'       = map searchQuery ges
        keys       = if Tx.null loc then ges' else loc:ges'
      in
        foldr (\ k m' -> Map.insertWith (<>) k [t] m') m keys

forICS :: IO [CalendarEvent]
forICS = do
  let sink = (eventSink :: ConduitT (Title BuilderType) Void IO [CalendarEvent])
  let conduits = orgSource .| normalConduit .| sink
  runConduit conduits

forICSVector :: IO (V.Vector CalendarEvent)
forICSVector = do
  let sink = (eventSinkVector ::
                 ConduitT (Title BuilderType) Void IO (V.Vector CalendarEvent))
  let conduits = orgSource .| normalConduit .| sink
  runConduit conduits

forGeocode :: TitleBuilder text =>
  ((Title text) -> Bool) -> IO (GeocodeMap text, GeocodeMap text)
forGeocode f = do
  let source ss = runConduit (CL.sourceList ss
                              .| CL.map runSelectType
                              .| locationSink)
  (resum01, strue) <- geocodeProducer f $$+ takeWhile isSelectTrue
  sfalse           <- resum01 $$+- CL.consume
  trueMap          <- source strue
  falseMap         <- source sfalse
  return (trueMap, falseMap)

---- utility ------------------------------------------------
takeWhile :: (a -> Bool) -> ConduitT a o IO [a]
takeWhile f = do
  a <- await
  case f <$> a of
    Just True -> (:) (fromJust a) <$> takeWhile f
    _         -> return []

toEvent :: TitleBuilder text =>
  Timestamp -> Title text -> CalendarEvent
toEvent stamp ttl =
  let
    (ttlLocation, _) = ttl ^. #location
    location = if Tx.null ttlLocation then Nothing else Just ttlLocation
  in
    eventDefault
  { eventDescription = desc
  , eventEnd         = stamp ^. #end <|> Just (stamp ^. #begin)
  , eventStart       = Just (stamp ^. #begin)
  , eventSummary     = Tx.dropWhileEnd (== ' ') (ttl ^. #label)
  , eventLocation    = location }
  where
    path' = ttl ^. #path
    para' = Tx.stripEnd $ toText $ ttl ^. #paragraph
    ps    = [path' == mempty, para' == mempty]
    sep   = if all not ps then "\n" else ""
    desc  =
      if all id ps
      then Nothing
      else Just $ foldMap (<>) [path', fromToken sep, para'] mempty

---- debug --------------------------------------------------
_test :: IO ()
_test = do
  let conduits = orgSource .| normalConduit .| locationSink
  r <- runConduit conduits :: IO (GeocodeMap Tx.Text)
  mapM_ (TxIO.putStrLn . fst) (Map.toList r)

_debugSink :: ConduitT (Title Tx.Text) Void IO ()
_debugSink = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  awaitForever (liftIO . TxIO.putStrLn . debug)
  where
    debug title = Tx.unwords [title ^. #label
                             , ":"
                             , title ^. #paragraph]

-- _debugPreTitleSink :: ConduitT Tx.Text Void IO ()
-- _debugPreTitleSink = do
--   liftIO $ Encoding.setLocaleEncoding Encoding.utf8
--   awaitForever $ \txt ->
--     liftIO $ print $ parse parser "" txt
