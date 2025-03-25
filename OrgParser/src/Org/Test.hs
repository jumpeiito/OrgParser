module Org.Test
  -- (testParse, testPrint)
  ()
where

import Org.Parse
import Org.Node
-- import Org.ICS
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

-- testParse :: IO ()
-- testParse = do
--   contents <- testData
--   mapM_ (putStrLn . show) contents

-- testData :: IO [OrgElement]
-- testData = do
--   contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
--   -- contents <- lines <$> readFile "c:/Users/kkr0133/Documents/OrgParser/OrgParser/src/Data/String/test.org"
--   -- contents <- lines <$> readFile "e:/Dropbox/notes.org"
--   -- contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/notes.org"
--   return $ concatMap ((mempty `fromRight`) . orgLineParse) contents

-- testPrint :: IO ()
-- testPrint = do
--   dx:dxs <- testData
--   let Just node = foldl (flip addNode) (makeOrgNode dx) dxs
--   putStrLn $ vcalendarToString $ nodeToVCalendar node
--     -- putStrLn $ path ++ ":" ++ (mempty `fromMaybe` showTitle e) ++ ", " ++ (show (length (timestamps e)))

--   -- print node
-- testWrite :: IO ()
-- testWrite = do
--   let icsfile = "./org.ics"
--   dx:dxs <- testData
--   let Just node = foldl (flip addNode) (makeOrgNode dx) dxs
--   let ics = vcalendarToString $ nodeToVCalendar node
--   writeFile icsfile ics

data YamlLine = YL Int OrgTitle

jNum     = M.fromList [(1, "１"), (2, "２"), (3, "３"), (4, "４"), (5, "５"),
                       (6, "６"), (7, "７"), (8, "８"), (9, "９"), (0, "０")]
jMaruNum = M.fromList [(1, "①"), (2, "②"), (3, "③"), (4, "④"), (5, "⑤"),
                       (6, "⑥"), (7, "⑦"), (8, "⑧"), (9, "⑨")]

instance Show YamlLine where
  show (YL counter a)
    | olevel a == 1 = bracket 1 mempty   mempty                 mempty
    | olevel a == 2 = bracket 2 mempty   (jNum M.! counter)     "．"
    | olevel a == 3 = bracket 3 "　"     (jMaruNum M.! counter) "　"
    | olevel a == 4 = bracket 4 "　　"   (jNum M.! counter)     "）"
    | olevel a == 5 = bracket 5 "　　　" "・"                   mempty
    where
      bracket lv spaces titlen after =
        concat ["- [", show lv, ", ", "\"", spaces, titlen
               , after, otitle a, "\", \"", oparagraph a, "\"]"]

addCounter :: Int -> M.Map Int Int -> M.Map Int Int
addCounter key m
  | key `M.notMember` m = M.insert key 1 m
  | otherwise =
      let deleted = foldr M.delete m $ filter (>key) $ M.keys m in
        M.adjustWithKey (\k' v' -> v' + 1) key deleted

testDocument :: IO ()
testDocument = do
  contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/2025議案書.org"
  let parsed = nodeCollectList (const True) $ orgLineNode contents
  (`runStateT` M.empty) $ do
    forM_ parsed $ \ttl -> do
      let lv = olevel ttl
      oldc <- get
      put $ addCounter lv oldc
      newc <- get
      let c = newc M.! lv
      lift $ putStrLn $ show $ YL c ttl
  return ()

main = testDocument
