{-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Org.Conduit
  (
    forICS
  )
where

import Control.Monad              (guard)
import Control.Monad.State        (lift, liftIO)
import Control.Monad.Trans.State.Strict
                                  (execStateT, StateT, get, put)
import Data.Maybe                 (isJust, fromJust, isNothing)
import Text.Megaparsec            (parse)
import Data.Conduit
import Data.Conduit.List          (sourceList, consume)
import Data.Extensible
import Control.Lens               hiding ((:>), oneOf, noneOf)
import System.Environment         (getEnv)
import System.Directory           (getDirectoryContents)
import qualified Data.List        as Dl
import qualified Data.Text        as Tx
import qualified Data.Text.IO     as TxIO
import qualified GHC.IO.Encoding  as Encoding

import Org.ParseText
import Org.Node                   (Node (..), build, scrap, scrapAll, toEvent)
import Org.GoogleCalendar.Event   (CalendarEvent (..))
------------------------------------------------------------
fileLines :: FilePath -> IO [Tx.Text]
fileLines fp = do
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
  lines    <- liftIO $ fileLines filepath
  mapM_ yield lines

archiveSource :: ConduitT () Tx.Text IO ()
archiveSource = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  files    <- liftIO orgArchiveFiles
  contents <- liftIO $ mapM fileLines files
  mapM_ yield (foldMap (++) contents [])

orgSource :: ConduitT () Tx.Text IO ()
orgSource = noteSource <> archiveSource
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
            (Nothing, Right (LL t)) -> loop (Just t)
            (Nothing, _)            -> loop current
            (Just c, Right (LL t))  -> do { yield c; loop (Just t) }
            (Just c, Right LB) ->
              loop $ Just (c & #paragraph .~ ((c ^. #paragraph) <> "\\r\\n\n"))
            (Just c, Right (LP ("LOCATION", l))) -> do
              loop $ Just (c & #location .~ l)
            (Just c, Right (LP ("PROPERTIES", l))) -> do
              loop $ Just c
            (Just c, Right (LP ("END", l))) -> do
              loop $ Just c
            (Just c, Right (LP ps)) -> do
              loop $ Just (c & #properties .~ ((c ^. #properties) <> [ps]))
            (Just c, Right (LO o)) -> do
              loop $ Just (o `mplusOther` c)

nodeConduit :: ConduitT Title (Node Title) IO ()
nodeConduit = do
  nodeTree <- loop None
  yield nodeTree
  where
    loop :: Node Title -> ConduitT Title (Node Title) IO (Node Title)
    loop current = do
      stream <- await
      case stream of
        Nothing    -> return current
        Just title -> loop $ build title current

titleBackConduit :: ConduitT (Node Title) Title IO ()
titleBackConduit = do
  node <- await
  case node of
    Just n  -> sourceList $ scrap n
    Nothing -> return ()

normalConduit :: ConduitT Tx.Text Title IO ()
normalConduit = do
  titleConduit
  .| nodeConduit
  .| titleBackConduit
------------------------------------------------------------
eventSink :: ConduitT Title Void IO [CalendarEvent]
eventSink = do
  titles <- consume
  let makeEvent title = map (`toEvent` title) $ title ^. #timestamps
  return $ concatMap makeEvent titles

debugSink :: ConduitT Title Void IO ()
debugSink = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  awaitForever (liftIO . TxIO.putStrLn . debug)
  where
    debug title = Tx.unwords [title ^. #label
                             , ":"
                             , title ^. #path]

debugPreTitleSink :: ConduitT Tx.Text Void IO ()
debugPreTitleSink = do
  liftIO $ Encoding.setLocaleEncoding Encoding.utf8
  awaitForever $ \txt ->
    liftIO $ print $ parse lineParse "" txt

forICS :: IO [CalendarEvent]
forICS = runConduit (orgSource .| normalConduit .| eventSink)
