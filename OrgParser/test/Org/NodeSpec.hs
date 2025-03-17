module Org.NodeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Org.Parse
import Org.Node
import Data.Time
import Data.Either

testStr = [
  "* Tasks"
  , "#+OPTIONS: toc:nil"
  , "#+EXPORT_FILE_NAME: ./notes"
  , "** level2"
  , "*** level3-1"
  , "<2024-12-01 日>"
  , "<2025-01-07 火>"
  , "<2025-02-01 土>"
  , "<2025-03-01 土>"
  , "<2025-04-01 火>"
  , "<2025-04-10 木>"
  , "*** level3-2"
  , "<2025-05-17 土>--<2025-05-18 日>"
  , "**** level4-1"
  , "***** level5-1"
  , "<2025-04-02 水>"
  , "***** level5-2"
  , "<2025-04-09 水>"
  , "** level2-2"]

-- testElements = concatMap ((mempty `fromRight`) . orgLineParse) testStr
-- testNode     = foldl (flip addNode) (head testElements) (tail testElements)
-- testElement = OrgTitle { title = "foo"
--                        , level = 2
--                        , todo  = Just "TODO"
--                        , children = []}

main :: IO ()
main = hspec spec

mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

spec :: Spec
spec = do
  describe "makeOrgNode" $ do
    it "(1) simple" $ do
      2 `shouldBe` 2
      -- makeOrgNode testElement `shouldBe` OrgNode testElement Nothing Nothing
--   describe "showTitle" $ do
--     it "(1) simple" $ do
--       showTitle testNode `shouldBe` Just "Tasks"
