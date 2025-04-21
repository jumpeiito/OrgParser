{-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Org.Conduit
  (
    forICS
  , forGeocode
  , documentSource
  , normalConduit
  , documentConduit
  )
where

import           Control.Monad.State
import           Text.Megaparsec            (parse)
import qualified Data.Map.Strict            as Map
import           Data.Conduit
import           Data.Conduit.List          (sourceList, consume)
import qualified Data.Conduit.List          as CL
import           Control.Lens               hiding ((:>), noneOf)
import           System.Environment         (getEnv)
import           System.Directory           (getDirectoryContents)
import qualified Data.List                  as Dl
import qualified Data.Text                  as Tx
import qualified Data.Text.IO               as TxIO
import qualified GHC.IO.Encoding            as Encoding
import           Org.ParseText
import           Org.Node                   (Node (..), build, scrap
                                            , scrapAll, toEvent
                                            , cut, pick, select)
import           Org.GoogleCalendar.Event   (CalendarEvent (..))
------------------------------------------------------------
type GeocodeMap = Map.Map Tx.Text [Title]

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
------------------------------------------------------------
titleConduit :: ConduitT Tx.Text Title IO ()
titleConduit = do
  lastTitle <- loop Nothing
  case lastTitle of
    Just lt -> yield lt
    Nothing -> return ()
  where
    loop :: Maybe Title -> ConduitT Tx.Text Title IO (Maybe Title)
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
              loop $ Just (c & #paragraph %~ (<> "\\r\\n\n"))
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

nodeConduitGenerator :: (Node Title -> b) -> ConduitT Title b IO ()
nodeConduitGenerator nodeModify = do
  nodeTree <- loop None
  yield (nodeModify nodeTree)
  where
    loop current = do
      stream <- await
      case stream of
        Nothing    -> return current
        Just title -> loop $ build title current

nodeConduit :: ConduitT Title (Node Title) IO ()
nodeConduit = nodeConduitGenerator (cut f)
  where
    f ttl = (ttl ^. #label == "プログラムメモ") && (ttl ^. #level == 2)

pickConduit :: (Title -> Bool) -> ConduitT Title (Node Title) IO ()
pickConduit f = nodeConduitGenerator (pick f)

selectConduit :: (Title -> Bool)
  -> ConduitT Title (Node Title, Node Title) IO ()
selectConduit f = nodeConduitGenerator (select f)

titleBackConduit :: ConduitT (Node Title) Title IO ()
titleBackConduit = do
  node <- await
  case node of
    Just n  -> sourceList $ scrap n
    Nothing -> return ()

titleBackAllConduit :: ConduitT (Node Title) Title IO ()
titleBackAllConduit = do
  node <- await
  case node of
    Just n  -> sourceList $ scrapAll n
    Nothing -> return ()

documentConduit :: ConduitT Tx.Text Title IO ()
documentConduit = do
  titleConduit
  .| nodeConduit
  .| titleBackAllConduit

normalConduit :: ConduitT Tx.Text Title IO ()
normalConduit = do
  titleConduit
  .| nodeConduit
  .| titleBackConduit

travelConduit :: (Title -> Bool) -> ConduitT Tx.Text Title IO ()
travelConduit f =
  titleConduit
  .| pickConduit f
  .| titleBackConduit
------------------------------------------------------------
eventSink :: ConduitT Title Void IO [CalendarEvent]
eventSink = do
  titles <- consume
  let makeEvent title = map (`toEvent` title) $ title ^. #timestamps
  return $ concatMap makeEvent titles

locationSink :: ConduitT Title Void IO GeocodeMap
locationSink = loop Map.empty
  where
    loop m = do
      stream <- await
      case stream of
        Nothing  -> return m
        Just ttl -> do
          let (loc, ges) = ttl ^. #location
          let ges' = map searchQuery ges :: [Tx.Text]
          let keys = if Tx.null loc then ges' else loc:ges'
          let map' = foldr (\k m' -> Map.insertWith (<>) k [ttl] m') m keys
          loop map'

_debugSink :: ConduitT Title Void IO ()
_debugSink = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  awaitForever (liftIO . TxIO.putStrLn . debug)
  where
    debug title = Tx.unwords [title ^. #label
                             , ":"
                             , title ^. #paragraph]

_debugPreTitleSink :: ConduitT Tx.Text Void IO ()
_debugPreTitleSink = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  awaitForever $ \txt ->
    liftIO $ print $ parse lineParse "" txt

forICS :: IO [CalendarEvent]
forICS = runConduit (orgSource .| normalConduit .| eventSink)

forGeocode :: (Title -> Bool) -> IO GeocodeMap
forGeocode f = do
  runConduit (orgSource
               .| travelConduit f
               .| locationSink)

_test :: IO ()
_test = do
  r <- runConduit (orgSource
                   .| normalConduit
                   .| locationSink)
  mapM_ (TxIO.putStrLn . fst) (Map.toList r)
