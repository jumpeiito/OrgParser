module Main (main) where

import Org.Parse
import Org.Node
-- import Org.ICS
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Either
import qualified Data.Map.Strict as M

import Prelude hiding (readFile, getLine, writeFile)
import System.Environment (getArgs)
import System.IO
import System.Win32.Encoding
import GHC.Encoding.UTF8

data YamlLine = YL Int OrgTitle

jNum     = M.fromList [(1, "１"), (2, "２"), (3, "３"), (4, "４"), (5, "５"),
                       (6, "６"), (7, "７"), (8, "８"), (9, "９"), (0, "０")]
jMaruNum = M.fromList [(1, "①"), (2, "②"), (3, "③"), (4, "④"), (5, "⑤"),
                       (6, "⑥"), (7, "⑦"), (8, "⑧"), (9, "⑨")]

instance Show YamlLine where
  show (YL counter a)
    | olevel a == 1 = bracket 1 mempty                 mempty
    | olevel a == 2 = bracket 2 (jNum M.! counter)     "．"
    | olevel a == 3 = bracket 3 (jMaruNum M.! counter) "　"
    | olevel a == 4 = bracket 4 (jNum M.! counter)     "）"
    | olevel a == 5 = bracket 5 "・"                   mempty
    where
      bracket lv titlen after =
        concat ["- [", show lv, ", ", "\"", titlen
               , after, otitle a, "\", \"", oparagraph a, "\"]"]

addCounter :: Int -> M.Map Int Int -> M.Map Int Int
addCounter key m
  | key `M.notMember` m = M.insert key 1 m
  | otherwise =
      let deleted = foldr M.delete m $ filter (>key) $ M.keys m in
        M.adjustWithKey (\k' v' -> v' + 1) key deleted

readUTF8File :: String -> IO String
readUTF8File filename = withFile filename ReadMode f
  where
    f h = hSetEncoding h utf8 >> hGetContents h

main :: IO ()
main = do
  -- contents <- lines <$> readFile "C:/users/jumpei/Documents/home/OrgFiles/2025議案書.org"
  args <- getArgs
  contents <- lines <$> readFile (args !! 0)
  let parsed = nodeCollectList (const True) $ orgLineNode contents
  (`runStateT` M.empty) $ do
    lift $ putStrLn "- [0, \"労働安全・労働条件改善、アスベスト問題解決をすすめる運動\", \"\"]"
    forM_ parsed $ \ttl -> do
      let lv = olevel ttl
      oldc <- get
      put $ addCounter lv oldc
      newc <- get
      let c = newc M.! lv
      lift $ putStrLn $ show $ YL c ttl
  return ()
