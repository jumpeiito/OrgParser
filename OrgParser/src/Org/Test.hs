module Org.Test
  -- (testParse, testPrint)
  ()
where

import Control.Monad
import Control.Monad.Reader
import System.Environment
import Data.Aeson
import Data.Maybe (fromMaybe)
import Org.Node
import Org.ICS
import Org.GoogleCalendar.Client
import Org.GoogleCalendar.Event

testData :: IO (Node OrgTitle)
testData = do
  orgdir   <- getEnv "ORG"
  let orgfile = orgdir ++ "/notes.org"
  contents <- lines <$> readFile orgfile
  return $ orgLineNode contents

testJsonPrint :: IO ()
testJsonPrint = do
  node   <- testData
  client <- clientFromFile
  -- forM_ byTime $ \(tmsp, ttl) -> do
  --   putStrLn $ otitle ttl ++ ":" ++ show (oend tmsp)
  let events    = nodeToCalendarEvents node
  forM_ events $ \e -> do
    putStrLn $ eventSummary e ++ (show (eventEnd e))
    print $ encode e
  -- mapM_ (putStrLn . show) events
  -- mapM_ (print . encode) events
  where
    timestampVtitle title = map (flip (,) title) $ otimestamps title

testInsert :: IO ()
testInsert = do
  node   <- testData
  client <- clientFromFile
  aToken <- aliveAccessToken `runReaderT` client
  let events    = nodeToCalendarEvents node
  insertEvent testEvent `runReaderT` (aToken, client)
