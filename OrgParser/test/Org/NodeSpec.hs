-- coding:utf-8
module Org.NodeSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Org.Parse
import Org.Node
-- import Data.Time
-- import Data.Either

-- testElements = concatMap ((mempty `fromRight`) . orgLineParse) testStr
-- testNode     = foldl (flip addNode) (head testElements) (tail testElements)
-- testElement = OrgTitle { title = "foo"
--                        , level = 2
--                        , todo  = Just "TODO"
--                        , children = []}
deftitle :: OrgTitle
deftitle = OrgTitle { otitle      = mempty
                    , olevel      = 1
                    , otodo       = Nothing
                    , otags       = []
                    , otimestamps = []
                    , oparagraph  = []
                    , oproperties = []
                    }

main :: IO ()
main = hspec spec

-- mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
-- mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

spec :: Spec
spec = do
  describe "Node" $ do
    it "(1) Functor instance" $ do
      (*2)   <$> (Node 3 None None :: Node Int) `shouldBe` (Node 6 None None)
      length <$> (Node "hoge" None None :: Node String) `shouldBe` (Node 4 None None)
      (2:)   <$> (Node [1] None None :: Node [Int]) `shouldBe` Node [2, 1] None None
      (*2)   <$> None `shouldBe` None
      (2:)   <$> None `shouldBe` None
    it "(2) Applicative instance" $ do
      (*) <$> (Node 3 (Node 4 None None) (Node 5 None None) :: Node Int)
          <*> (Node 10 (Node 20 None None) (Node 30 None None) :: Node Int)
        `shouldBe`
        Node 30 (Node 80 None None) (Node 150 None None)
    it "(3) EQN instance" $ do
      let node1 = pure deftitle
      let node2 = pure $ deftitle { olevel = 2 }
      let node3 = pure $ deftitle { olevel = 1 }
      EQN None  == EQN None  `shouldBe` True
      EQN None  == EQN node1 `shouldBe` False
      EQN node1 == EQN None  `shouldBe` False
      EQN node1 == EQN node2 `shouldBe` False
      EQN node1 == EQN node3 `shouldBe` True
      EQN None  `compare` EQN None  `shouldBe` EQ
      EQN None  `compare` EQN node1 `shouldBe` LT
      EQN node2 `compare` EQN None  `shouldBe` GT
      EQN node1 `compare` EQN node2 `shouldBe` LT
      EQN node2 `compare` EQN node3 `shouldBe` GT
      EQN node1 `compare` EQN node3 `shouldBe` EQ
  --     (*2) <$> (Node 3 (Node 4 None None) None) `shouldBe` Node 6 (Node 8 None None) None
  -- describe "Other" $ do
  --   it "it" $ 3 `shouldBe` 3
