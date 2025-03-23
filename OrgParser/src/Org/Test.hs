module Org.Test
  (testParse, testPrint)
where

import Org.Parse
import Org.Node
import Org.ICS
import Control.Monad
import Data.Maybe
import Data.Either

testParse :: IO ()
testParse = do
  contents <- testData
  mapM_ (putStrLn . show) contents

testData :: IO [OrgElement]
testData = do
  contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
  -- contents <- lines <$> readFile "c:/Users/kkr0133/Documents/OrgParser/OrgParser/src/Data/String/test.org"
  -- contents <- lines <$> readFile "e:/Dropbox/notes.org"
  -- contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
  return $ concatMap ((mempty `fromRight`) . orgLineParse) contents

testPrint :: IO ()
testPrint = do
  dx:dxs <- testData
  let Just node = foldl (flip addNode) (makeOrgNode dx) dxs
  putStrLn $ vcalendarToString $ nodeToVCalendar node
    -- putStrLn $ path ++ ":" ++ (mempty `fromMaybe` showTitle e) ++ ", " ++ (show (length (timestamps e)))

  -- print node
testWrite :: IO ()
testWrite = do
  let icsfile = "./org.ics"
  dx:dxs <- testData
  let Just node = foldl (flip addNode) (makeOrgNode dx) dxs
  let ics = vcalendarToString $ nodeToVCalendar node
  writeFile icsfile ics
