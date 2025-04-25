{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module Org.Parse.TimeSpec (main, spec) where

import           Test.Hspec
import           Control.Lens           hiding ((<:), noneOf)
import           Data.Extensible
import           Data.Maybe             (catMaybes)
import           Data.Tagged
import qualified Data.Text              as Tx
import           Text.Megaparsec
import           Org.Parse.Utility
import           Org.Parse.Time

numText :: (Num a, Show a) => a -> Tx.Text
numText = Tx.pack . show
numText2 :: (Num a, Show a, Ord a) => a -> Tx.Text
numText2 =
  let
    show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
  in
    Tx.pack . show'

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "yearP" $ do
    it "(1)" $ do
      let years = [2024..2099] :: [Tagged "Year" Integer]
      catMaybes [ parseMaybe yearP (numText . untag $ y)
                | y <- years ]
        `shouldBe`
        years
    it "(2)" $ do
      parse yearP "" "20245" `shouldBe` Right 2024
    it "(3)" $ do
      parseMaybe yearP "52024" `shouldBe` Nothing
    it "(4)" $ do
      parseMaybe yearP "0" `shouldBe` Nothing
    it "(5)" $ do
      parseMaybe yearP "0000" `shouldBe` Nothing
    it "(6)" $ do
      parseMaybe yearP "3000" `shouldBe` Nothing
  describe "monthP" $ do
    it "(1)" $ do
      let months = [1..12] :: [Tagged "Month" Int]
      catMaybes [ parseMaybe monthP (numText2 . untag $ m)
                | m <- months]
        `shouldBe` months
    it "(2)" $ do
      parseMaybe monthP "03" `shouldBe` Just 3
    it "(3)" $ do
      parse monthP "" "039" `shouldBe` Right 3
    it "(4)" $ do
      parseMaybe monthP "39" `shouldBe` Nothing
  describe "dayP" $ do
    it "(1)" $ do
      let days = [1..31] :: [Tagged "Day" Int]
      let show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
      catMaybes [ parseMaybe dayP (numText2 . untag $ m) | m <- days]
        `shouldBe` days
    it "(2)" $ do
      parseMaybe dayP "03" `shouldBe` Just 3
    it "(3)" $ do
      parse dayP "" "100" `shouldBe` Right 10
    it "(4)" $ do
      parseMaybe dayP "39" `shouldBe` Nothing
  describe "hourP" $ do
    it "(1)" $ do
      let hours = [1..23] :: [Tagged "Hour" Int]
      let show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
      catMaybes [ parseMaybe hourP (numText2 . untag $ m) | m <- hours]
        `shouldBe` hours
    it "(2)" $ do
      parseMaybe hourP "03" `shouldBe` Just 3
    it "(3)" $ do
      parse hourP "" "100" `shouldBe` Right 10
    it "(4)" $ do
      parseMaybe hourP "39" `shouldBe` Nothing
  describe "minuteP" $ do
    it "(1)" $ do
      let minutes = [1..23] :: [Tagged "Minute" Int]
      let show' s = if (s >= 1 && s <= 9) then "0" ++ show s else show s
      catMaybes [ parseMaybe minuteP (numText2 . untag $ m) | m <- minutes]
        `shouldBe` minutes
    it "(2)" $ do
      parseMaybe minuteP "03" `shouldBe` Just 3
    it "(3)" $ do
      parse minuteP "" "100" `shouldBe` Right 10
    it "(4)" $ do
      parseMaybe minuteP "690" `shouldBe` Nothing
  describe "timeP" $ do
    it "(1)" $ do
      parseMaybe timeP "10:00" `shouldBe`
        (Just ((Tagged 10, Tagged 0), Nothing)
         :: Maybe (Time, Maybe Time))
    it "(2)" $ do
      parseMaybe timeP "23:33" `shouldBe`
        (Just ((Tagged 23, Tagged 33), Nothing)
         :: Maybe (Time, Maybe Time))
    it "(3)" $ do
      parseMaybe timeP "10:00-17:00" `shouldBe`
        (Just ((Tagged 10, Tagged 0), Just (Tagged 17, Tagged 0))
         :: Maybe (Time, Maybe Time))
    it "(4)" $ do
      parseMaybe timeP "10:20-17:11" `shouldBe`
        (Just ((Tagged 10, Tagged 20), Just (Tagged 17, Tagged 11))
         :: Maybe (Time, Maybe Time))
    it "(5)" $ do
      parse timeP "" "11:000" `shouldBe`
        Right (((Tagged 11, Tagged 0), Nothing) :: (Time, Maybe Time))
    it "(6)" $ do
      parseMaybe timeP "31:00" `shouldBe`
        (Nothing :: Maybe (Time, Maybe Time))
  describe "dateYMDP" $ do
    it "(1)" $ do
      parseMaybe dateYMDP "2025-04-01" `shouldBe`
        (Just (Tagged 2025, Tagged 4, Tagged 1)
         :: Maybe (Tagged "Year" Integer, Tagged "Month" Int, Tagged "Day" Int))
    it "(2)" $ do
      parseMaybe dateYMDP "2023-04-01" `shouldBe` Nothing
    it "(3)" $ do
      parseMaybe dateYMDP "3000-04-01" `shouldBe` Nothing
    it "(4)" $ do
      parseMaybe dateYMDP "2025-4-01" `shouldBe` Nothing
    it "(5)" $ do
      parseMaybe dateYMDP "2025-04-1" `shouldBe` Nothing
  describe "timestampTypeP" $ do
    let f = timestampTypeP
    it "(1)" $ do
      parseMaybe f "SCHEDULED: " `shouldBe` Just Scheduled
    it "(2)" $ do
      parseMaybe f "DEADLINE: " `shouldBe` Just Deadline
    it "(3)" $ do
      parseMaybe f "CLOSED: " `shouldBe` Just Closed
    it "(4)" $ do
      parseMaybe f "" `shouldBe` Just Normal
  describe "timestampCoreP" $ do
    it "(1)" $ do
      parseMaybe timestampCoreP "2025-04-10 金 10:00"
        `shouldBe`
        Just (#begin      @= makeUTC 2025 4 10 10 0
              <: #datetype @= Normal
              <: #active   @= True
              <: #end      @= Nothing
              <: nil)
    it "(2)" $ do
      parseMaybe timestampCoreP "2023-04-10 金 10:00" `shouldBe` Nothing
    it "(3)" $ do
      parseMaybe timestampCoreP "2025-4-10 金 10:00" `shouldBe` Nothing
    it "(4)" $ do
      parseMaybe timestampCoreP "2025-04-39 金 10:00" `shouldBe` Nothing
    it "(5)" $ do
      parseMaybe timestampCoreP "2025-04-10 金 10:00-12:00"
        `shouldBe`
        Just (#begin      @= makeUTC 2025 4 10 10 0
              <: #datetype @= Normal
              <: #active   @= True
              <: #end      @= Just (makeUTC 2025 4 10 12 0)
              <: nil)
    it "(6)" $ do
      parseMaybe timestampCoreP "2025-04-10 金"
        `shouldBe`
        Just (#begin      @= makeUTC 2025 4 10 0 0
              <: #datetype @= Normal
              <: #active   @= True
              <: #end      @= Nothing
              <: nil)
    it "(7)" $ do
      parseMaybe timestampCoreP "2023-04-10 金" `shouldBe` Nothing
  describe "timestampSingleP" $ do
    it "(1)" $ do
      parseMaybe timestampSingleP "<2025-04-10 月 10:00>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Nothing
               <: nil)
    it "(2)" $ do
      parseMaybe timestampSingleP "<2025-04-10 月 10:00-12:00>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(3)" $ do
      parseMaybe timestampSingleP "[2025-04-10 月 10:00-12:00]"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= False
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(4)" $ do
      parseMaybe timestampSingleP "[2025-04-10 月]"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Normal
               <: #active   @= False
               <: #end      @= Nothing <: nil)
  describe "timestampP" $ do
    it "(1)" $ do
      parseMaybe timestampP "<2025-04-10 月 10:00>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Nothing
               <: nil)
    it "(2)" $ do
      parseMaybe timestampP "<2025-04-10 月>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Nothing <: nil)
    it "(3)" $ do
      parseMaybe timestampP "<2025-04-10 月 10:00-12:00>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(4)" $ do
      parseMaybe timestampP "SCHEDULED: <2025-04-10 月 10:00-12:00>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Scheduled
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 10 12 0) <: nil)
    it "(5)" $ do
      parseMaybe timestampP "CLOSED: <2025-04-10 月>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Closed
               <: #active   @= True
               <: #end      @= Nothing <: nil)
    it "(6)" $ do
      parseMaybe timestampP "CLOSED: <2025-04-10 月 10:00>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Closed
               <: #active   @= True
               <: #end      @= Nothing <: nil)
    it "(7)" $ do
      parseMaybe timestampP "<2025-04-10 月 10:00>-<2025-04-11 火 17:00>"
        `shouldBe`
        Just (#begin       @= makeUTC 2025 4 10 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 11 17 0) <: nil)
    it "(8)" $ do
      parseMaybe timestampP "<2025-04-10 月>-<2025-04-11 火>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 4 10 0 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 4 11 0 0) <: nil)
    it "(9)" $ do
      parseMaybe timestampP "<2025-05-03 土 10:00-11:40>"
        `shouldBe`
        Just  (#begin       @= makeUTC 2025 5 3 10 0
               <: #datetype @= Normal
               <: #active   @= True
               <: #end      @= Just (makeUTC 2025 5 3 11 40) <: nil)
