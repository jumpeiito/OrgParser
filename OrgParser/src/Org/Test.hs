module Org.Test
  -- (testParse, testPrint)
  ()
where

import Org.Node
import Org.ICS

testData :: IO (Node OrgTitle)
testData = do
  contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
  -- contents <- lines <$> readFile "e:/Dropbox/notes.org"
  return $ orgLineNode contents

testInsert :: IO ()
testInsert = do
  node <- testData
  let collecter = nodeCollectList normalFilter node
  let byTime    = foldMap timestampVtitle collecter
  let events    = map (uncurry nodeToCalendarEvent) byTime
  updateGoogleCalendar events
  where
    timestampVtitle title = map (flip (,) title) $ otimestamps title
