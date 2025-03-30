module Org.Test
  -- (testParse, testPrint)
  ()
where

import Org.Parse
import Org.Node
import Org.ICS
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

-- testParse :: IO ()
-- testParse = do
--   contents <- testData
--   mapM_ (putStrLn . show) contents

testData :: IO (Node OrgTitle)
testData = do
  contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
  -- contents <- lines <$> readFile "e:/Dropbox/notes.org"
  return $ orgLineNode contents

testInsert :: IO ()
-- testInsert = undefined
testInsert = do
  node <- testData
  let collecter = nodeCollectList normalFilter node
  let byTime    = foldMap timestampVtitle collecter
  -- updateGoogleCalendar titleList
  let events    = map (uncurry nodeToCalendarEvent) byTime
  print $ head events
  where
    timestampVtitle title = map (flip (,) title) $ otimestamps title
