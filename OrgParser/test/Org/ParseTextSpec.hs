{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Org.ParseTextSpec (main, spec) where

import Test.Hspec
-- import Test.QuickCheck
import Org.ParseText
import Data.Time
import Data.Either (isLeft, rights)
import GHC.Base (Alternative)
import Data.Time
import Data.Void
import Data.Coerce
import Data.Tagged
import qualified Data.Text as Tx
import qualified Data.List as Dl
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Extensible
import Control.Lens hiding ((:>), oneOf, noneOf)

type Parser    = Parsec Void Tx.Text

isLL, isLP, isLB, isLO :: Line -> Bool
isLL (LL _) = True
isLL _ = False
isLP (LP _) = True
isLP _ = False
isLB LB = True
isLB _ = False
isLO (LO _) = True
isLO _ = False

main :: IO ()
main = hspec spec

-- mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
-- mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))
numText :: (Num a, Show a) => a -> Tx.Text
numText = Tx.pack . show
numText2 :: (Num a, Show a, Ord a) => a -> Tx.Text
numText2 =
  let
    show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
  in
    Tx.pack . show'

spec :: Spec
spec = do
  describe "tagsP" $ do
    it "(1)" $ do
      parse tagsP "" ":hoge:" `shouldBe` Right ["hoge"]
    it "(2)" $ do
      parse tagsP "" ":hoge:foo:" `shouldBe` Right ["hoge", "foo"]
    it "(3)" $ do
      isLeft (parse tagsP "" "") `shouldBe` True
    it "(4)" $ do
      isLeft (parse tagsP "" "::") `shouldBe` True
  describe "yearP" $ do
    it "(1)" $ do
      let years = [2024..2099] :: [Tagged "Year" Integer]
      rights [ parse yearP "" (numText . untag $ y) | y <- years ] `shouldBe` years
    it "(2)" $ do
      parse yearP "" "20245" `shouldBe` Right 2024
    it "(3)" $ do
      isLeft (parse yearP "" "52024") `shouldBe` True
    it "(4)" $ do
      isLeft (parse yearP "" "0") `shouldBe` True
    it "(5)" $ do
      isLeft (parse yearP "" "0000") `shouldBe` True
    it "(6)" $ do
      isLeft (parse yearP "" "3000") `shouldBe` True
  describe "monthP" $ do
    it "(1)" $ do
      let months = [1..12] :: [Tagged "Month" Int]
      rights [ parse monthP "" (numText2 . untag $ m) | m <- months]
        `shouldBe` months
    it "(2)" $ do
      parse monthP "" "03" `shouldBe` Right 3
    it "(3)" $ do
      parse monthP "" "039" `shouldBe` Right 3
    it "(4)" $ do
      isLeft (parse monthP "" "39") `shouldBe` True
  describe "dayP" $ do
    it "(1)" $ do
      let days = [1..31] :: [Tagged "Day" Int]
      let show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
      rights [ parse dayP "" (numText2 . untag $ m) | m <- days]
        `shouldBe` days
    it "(2)" $ do
      parse dayP "" "03" `shouldBe` Right 3
    it "(3)" $ do
      parse dayP "" "100" `shouldBe` Right 10
    it "(4)" $ do
      isLeft (parse dayP "" "39") `shouldBe` True
  describe "hourP" $ do
    it "(1)" $ do
      let hours = [1..23] :: [Tagged "Hour" Int]
      let show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
      rights [ parse hourP "" (numText2 . untag $ m) | m <- hours]
        `shouldBe` hours
    it "(2)" $ do
      parse hourP "" "03" `shouldBe` Right 3
    it "(3)" $ do
      parse hourP "" "100" `shouldBe` Right 10
    it "(4)" $ do
      isLeft (parse hourP "" "39") `shouldBe` True
  describe "minuteP" $ do
    it "(1)" $ do
      let minutes = [1..23] :: [Tagged "Minute" Int]
      let show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
      rights [ parse minuteP "" (numText2 . untag $ m) | m <- minutes]
        `shouldBe` minutes
    it "(2)" $ do
      parse minuteP "" "03" `shouldBe` Right 3
    it "(3)" $ do
      parse minuteP "" "100" `shouldBe` Right 10
    it "(4)" $ do
      isLeft (parse minuteP "" "690") `shouldBe` True
  describe "timeP" $ do
    it "(1)" $ do
      parse timeP "" "10:00" `shouldBe` Right ((Tagged 10, Tagged 0), Nothing)
    it "(2)" $ do
      parse timeP "" "23:33" `shouldBe` Right ((Tagged 23, Tagged 33), Nothing)
    it "(3)" $ do
      parse timeP "" "10:00-17:00"
        `shouldBe` Right ((Tagged 10, Tagged 0), Just (Tagged 17, Tagged 0))
    it "(4)" $ do
      parse timeP "" "10:20-17:11"
        `shouldBe` Right ((Tagged 10, Tagged 20), Just (Tagged 17, Tagged 11))
    it "(5)" $ do
      parse timeP "" "11:000" `shouldBe` Right ((Tagged 11, Tagged 0), Nothing)
    it "(6)" $ do
      isLeft (parse timeP "" "31:00") `shouldBe` True
  describe "dateYMDP" $ do
    it "(1)" $ do
      parse dateYMDP "" "2025-04-01" `shouldBe` Right (Tagged 2025, Tagged 4, Tagged 1)
    it "(2)" $ do
      isLeft (parse dateYMDP "" "2023-04-01") `shouldBe` True
    it "(3)" $ do
      isLeft (parse dateYMDP "" "3000-04-01") `shouldBe` True
    it "(4)" $ do
      isLeft (parse dateYMDP "" "2025-4-01") `shouldBe` True
    it "(5)" $ do
      isLeft (parse dateYMDP "" "2025-04-1") `shouldBe` True
  describe "timestampTypeP" $ do
    it "(1)" $ do
      parse timestampTypeP "" "SCHEDULED: " `shouldBe` Right Scheduled
    it "(2)" $ do
      parse timestampTypeP "" "DEADLINE: " `shouldBe` Right Deadline
    it "(3)" $ do
      parse timestampTypeP "" "CLOSED: " `shouldBe` Right Closed
    it "(4)" $ do
      parse timestampTypeP "" "" `shouldBe` Right Normal
  describe "orgstarsP" $ do
    it "(1)" $ do
      parse orgstarsP "" "** " `shouldBe` Right 2
      parse orgstarsP "" "*** " `shouldBe` Right 3
      parse orgstarsP "" "**** " `shouldBe` Right 4
      parse orgstarsP "" "***** " `shouldBe` Right 5
    it "(2)" $ do
      isLeft (parse orgstarsP "" " *") `shouldBe` True
  describe "todoP" $ do
    it "(1)" $ do
      parse todoP "" "TODO" `shouldBe` Right (Just "TODO")
      parse todoP "" "DONE" `shouldBe` Right (Just "DONE")
      parse todoP "" "WAIT" `shouldBe` Right (Just "WAIT")
      parse todoP "" "PEND" `shouldBe` Right (Just "PEND")
    it "(2)" $ do
      parse todoP "" "" `shouldBe` Right Nothing

  describe "timestampCoreP" $ do
    it "(1)" $ do
      parse timestampCoreP "" "2025-04-10 金 10:00"
        `shouldBe`
        Right (#begin      @= makeUTC 2025 4 10 10 0
              <: #datetype @= Normal
              <: #active   @= True
              <: #end      @= Nothing
              <: nil)
    it "(2)" $ do
      isLeft (parse timestampCoreP "" "2023-04-10 金 10:00") `shouldBe` True
    it "(3)" $ do
      isLeft (parse timestampCoreP "" "2025-4-10 金 10:00") `shouldBe` True
    it "(4)" $ do
      isLeft (parse timestampCoreP "" "2025-04-39 金 10:00") `shouldBe` True
    it "(5)" $ do
      parse timestampCoreP "" "2025-04-10 金 10:00-12:00"
        `shouldBe`
        Right (#begin      @= makeUTC 2025 4 10 10 0
              <: #datetype @= Normal
              <: #active   @= True
              <: #end      @= Just (makeUTC 2025 4 10 12 0)
              <: nil)
    it "(6)" $ do
      parse timestampCoreP "" "2025-04-10 金"
        `shouldBe`
        Right (#begin      @= makeUTC 2025 4 10 0 0
              <: #datetype @= Normal
              <: #active   @= True
              <: #end      @= Nothing
              <: nil)
    it "(7)" $ do
      isLeft (parse timestampCoreP "" "2023-04-10 金") `shouldBe` True
  describe "timestampSingleP" $ do
    it "(1)" $ do
      parse timestampSingleP "" "<2025-04-10 月 10:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Nothing
               <: nil)
    it "(2)" $ do
      parse timestampSingleP "" "<2025-04-10 月 10:00-12:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(3)" $ do
      parse timestampSingleP "" "[2025-04-10 月 10:00-12:00]"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= False
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(4)" $ do
      parse timestampSingleP "" "[2025-04-10 月]"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Normal
               <: #active   @= False
               <: #end      @= Nothing <: nil)
  describe "timestampP" $ do
    it "(1)" $ do
      parse timestampP "" "<2025-04-10 月 10:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Nothing
               <: nil)
    it "(2)" $ do
      parse timestampP "" "<2025-04-10 月>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Nothing <: nil)
    it "(3)" $ do
      parse timestampP "" "<2025-04-10 月 10:00-12:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(4)" $ do
      parse timestampP "" "SCHEDULED: <2025-04-10 月 10:00-12:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Scheduled
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(5)" $ do
      parse timestampP "" "CLOSED: <2025-04-10 月>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Closed
               <: #active   @= True
               <: #end      @= Nothing <: nil)
    it "(6)" $ do
      parse timestampP "" "CLOSED: <2025-04-10 月 10:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Closed
               <: #active   @= True
               <: #end      @= Nothing <: nil)
    it "(7)" $ do
      parse timestampP "" "<2025-04-10 月 10:00>-<2025-04-11 火 17:00>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 11 17 0) <: nil)
    it "(8)" $ do
      parse timestampP "" "<2025-04-10 月>-<2025-04-11 火>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 11 0 0) <: nil)
    it "(9)" $ do
      parse timestampP "" "<2025-05-03 土 10:00-11:40>"
        `shouldBe`
        Right (#begin       @= makeUTC 2025 5 3 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 5 3 11 40) <: nil)
  describe "titleP" $ do
    it "(1)" $ do
      parse titleP "" "**** hoge"
        `shouldBe`
       Right (#label         @= "hoge"
              <: #level      @= 4
              <: #todo       @= Nothing
              <: #tags       @= []
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(2)" $ do
      parse titleP "" "*** [10/10] hoge"
        `shouldBe`
       Right (#label         @= "hoge"
              <: #level      @= 3
              <: #todo       @= Nothing
              <: #tags       @= []
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(3)" $ do
      parse titleP "" "*** TODO [10/10] hoge"
        `shouldBe`
       Right (#label         @= "hoge"
              <: #level      @= 3
              <: #todo       @= Just "TODO"
              <: #tags       @= []
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(4)" $ do
      parse titleP "" "*** DONE [10/10] hoge    "
        `shouldBe`
       Right (#label         @= "hoge"
              <: #level      @= 3
              <: #todo       @= Just "DONE"
              <: #tags       @= []
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(5)" $ do
      parse titleP "" "*** DONE [10/10] ho  ge    "
        `shouldBe`
       Right (#label         @= "ho  ge"
              <: #level      @= 3
              <: #todo       @= Just "DONE"
              <: #tags       @= []
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(6)" $ do
      parse titleP "" "*** DONE [10/10] ho  ge    ::"
        `shouldBe`
       Right (#label         @= "ho  ge    ::"
              <: #level      @= 3
              <: #todo       @= Just "DONE"
              <: #tags       @= []
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(7)" $ do
      parse titleP "" "*** DONE [10/10] ho  ge    :hoge:"
        `shouldBe`
       Right (#label         @= "ho  ge"
              <: #level      @= 3
              <: #todo       @= Just "DONE"
              <: #tags       @= ["hoge"]
              <: #timestamps @= []
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(8)" $ do
      parse titleP "" "*** DONE [10/10] ho  ge <2024-12-01 土 12:00>"
        `shouldBe`
        Right (#label         @= "ho  ge"
               <: #level      @= 3
               <: #todo       @= Just "DONE"
               <: #tags       @= []
               <: #timestamps @= [ #begin       @= makeUTC 2024 12 1 12 0
                                   <: #datetype @= Normal
                                   <: #active   @= True
                                   <: #end      @= Nothing
                                   <: nil]
               <: #paragraph  @= mempty
               <: #properties @= mempty
               <: #location   @= mempty
               <: #path       @= mempty
               <: nil)
    it "(9)" $ do
      parse titleP "" "*** DONE [10/10] ho ge  <2024-12-01 火 12:00> :hoge:"
        `shouldBe`
       Right (#label         @= "ho ge"
              <: #level      @= 3
              <: #todo       @= Just "DONE"
              <: #tags       @= ["hoge"]
              <: #timestamps @= [ #begin       @= makeUTC 2024 12 1 12 0
                                  <: #datetype @= Normal
                                  <: #active   @= True
                                  <: #end      @= Nothing
                                  <: nil]
              <: #paragraph  @= mempty
              <: #properties @= mempty
              <: #location   @= mempty
              <: #path       @= mempty
              <: nil)
    it "(10)" $ do
      parse titleP "" "*** ho ge  <2024-12-01 木 12:00>"
        `shouldBe`
        Right (#label         @= "ho ge"
               <: #level      @= 3
               <: #todo       @= Nothing
               <: #tags       @= []
               <: #timestamps @= [ #begin       @= makeUTC 2024 12 1 12 0
                                   <: #datetype @= Normal
                                   <: #active   @= True
                                   <: #end      @= Nothing
                                   <: nil]
               <: #paragraph  @= mempty
               <: #properties @= mempty
               <: #location   @= mempty
               <: #path       @= mempty
               <: nil)
    it "(11)" $ do
      parse titleP "" "***** DONE 加入申込書の納品 :片岡:"
        `shouldBe`
        Right (#label         @= "加入申込書の納品"
                <: #level      @= 5
                <: #todo       @= Just "DONE"
                <: #tags       @= ["片岡"]
                <: #timestamps @= []
                <: #paragraph  @= mempty
                <: #properties @= mempty
                <: #location   @= mempty
                <: #path       @= mempty
                <: nil)
    it "(12)" $ do
      parse titleP "" "***** 石見銀山"
        `shouldBe`
        Right (#label         @= "石見銀山"
                <: #level      @= 5
                <: #todo       @= Nothing
                <: #tags       @= []
                <: #timestamps @= []
                <: #paragraph  @= mempty
                <: #properties @= mempty
                <: #location   @= mempty
                <: #path       @= mempty
                <: nil)
  describe "otherP" $ do
    it "(1)" $ do
      let
        p :: Parser ([Token Tx.Text], Timestamp)
        p = someTill_ anySingle timestampP
      parse p "" " hoge  <2025-04-10 月 10:00>"
        `shouldBe`
        Right (" hoge  "
               , #begin @= makeUTC 2025 4 10 10 0
                 <: #datetype @= Normal
                 <: #active @= True
                 <: #end @= Nothing
                 <: nil)
    it "(2)" $ do
      parse otherP "" "<2025-04-10 月 10:00> CLOSED: <2026-04-01 火>"
        `shouldBe`
        Right ( #timestamps @= [ #begin @= makeUTC 2025 4 10 10 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil
                               , #begin @= makeUTC 2026 4 1 0 0
                                 <: #datetype @= Closed
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil]
                <: #others @= mempty
                <: nil)
    it "(3)" $ do
      parse otherP "" "hoge <2025-04-10 月 10:00>"
        `shouldBe`
        Right ( #timestamps @= [ #begin @= makeUTC 2025 4 10 10 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil ]
                <: #others @= ["hoge "]
                <: nil)
    it "(4)" $ do
      parse otherP "" "hoge <2025-04-10 月> foo"
        `shouldBe`
        Right ( #timestamps @= [ #begin @= makeUTC 2025 4 10 0 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil ]
                <: #others @= ["hoge ", "foo"]
                <: nil)
    it "(5)" $ do
      parse otherP "" "hoge <2025-04-10 月> foo [[http://google.co.jp][Google]] buz! SCHEDULED: <2025-04-11 火 12:00-17:00>"
        `shouldBe`
        Right ( #timestamps @= [ #begin @= makeUTC 2025 4 10 0 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil
                               , #begin @= makeUTC 2025 4 11 12 0
                                 <: #datetype @= Scheduled
                                 <: #active @= True
                                 <: #end @= Just (makeUTC 2025 4 11 17 0)
                                 <: nil]
                <: #others @= ["hoge ", "foo ", "<a href=\"http://google.co.jp\">Google</a>", "buz! "]
                <: nil)
  describe "lineParse" $ do
    it "(1)" $ do
      isLL <$> parse lineParse "" "**** hoge" `shouldBe` Right True
      isLL <$> parse lineParse "" " *** hoge" `shouldBe` Right False
      isLL <$> parse lineParse "" "*** [10/10] hoge" `shouldBe` Right True
      isLL <$> parse lineParse "" "*** TODO [10/10] hoge" `shouldBe` Right True
      isLL <$> parse lineParse "" "*** DONE [10/10] hoge    " `shouldBe` Right True
      isLL <$> parse lineParse "" "*** DONE [10/10] ho  ge    " `shouldBe` Right True
      isLL <$> parse lineParse "" "*** DONE [10/10] ho  ge    ::" `shouldBe` Right True
      isLL <$> parse lineParse "" "** DONE [10/10] ho  ge    :hoge:" `shouldBe` Right True
      isLL <$> parse lineParse "" "*** DONE [10/10] ho  ge <2024-12-01 土 12:00>" `shouldBe` Right True
      isLL <$> parse lineParse "" "*** DONE [10/10] ho ge  <2024-12-01 火 12:00> :hoge:" `shouldBe` Right True
      isLL <$> parse lineParse "" "*** ho ge  <2024-12-01 木 12:00>" `shouldBe` Right True
      isLL <$> parse lineParse "" "***** DONE 加入申込書の納品 :片岡:" `shouldBe` Right True
    it "(2)" $ do
      parse lineParse "" "***** 石見銀山" `shouldBe`
        Right (LL (#label      @= "石見銀山"
                <: #level      @= 5
                <: #todo       @= Nothing
                <: #tags       @= []
                <: #timestamps @= []
                <: #paragraph  @= mempty
                <: #properties @= mempty
                <: #location   @= mempty
                <: #path       @= mempty
                <: nil))
      -- parse lineParse "" ":PROPERTIES:" `shouldBe`
      --   Right (LP ("PROPERTIES", ""))
      parse lineParse "" ":LOCATION: 石見銀山" `shouldBe`
        Right (LP ("LOCATION", "石見銀山"))
      parse lineParse "" ":END:" `shouldBe`
        Right (LP ("END", ""))
      parse lineParse "" "<2025-05-03 土 10:00-11:40>" `shouldBe`
        Right (LO (#timestamps @= [ #begin @= makeUTC 2025 5 3 10 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= (Just $ makeUTC 2025 5 3 11 40)
                                 <: nil]
                   <: #others @= mempty
                   <: nil))

      parse lineParse "" "[[https://www.google.com/maps/dir][自宅～石見銀山]]"
        `shouldBe`
        Right (LO (#timestamps @= mempty
                   <: #others @= [Tx.pack "<a href=\"https://www.google.com/maps/dir\">自宅～石見銀山</a>"]
                  <: nil))
  -- it "(2)" $ do
    --   parse otherP "" "CLOSED: [2025-03-24 月 11:58] SCHEDULED: <2025-03-24 月>"
    --     `shouldBe`
    --     Right (#timestamps @= [ #begin       @= makeUTC 2025 3 24 11 58
    --                             <: #datetype @= Normal
    --                             <: #active   @= False
    --                             <: #end      @= Nothing <: nil
    --                           , #begin       @= makeUTC 2025 3 24 0 0
    --                             <: #datetype @= Normal
    --                             <: #active   @= True
    --                             <: #end      @= Nothing <: nil]
    --            <: #others @= mempty
    --            <: #link @= mempty
    --            <: nil)


