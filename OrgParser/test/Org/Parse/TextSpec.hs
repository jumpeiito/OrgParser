{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module Org.Parse.TextSpec (main, spec) where

import           Test.Hspec
import           Control.Lens           hiding ((<:), noneOf)
import           Data.Extensible
import qualified Data.Text              as Tx
import           Data.Void              (Void)
import           Data.Either            (isLeft)
import           Data.Foldable          (fold)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Org.Parse.Utility
import           Org.Parse.Time
import           Org.Parse.Text

isLL, isLP, isLB, isLO :: Line Text -> Bool
isLL (LL _) = True
isLL _      = False
isLP (LP _) = True
isLP _      = False
isLB LB     = True
isLB _      = False
isLO (LO _) = True
isLO _      = False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let f = tagsP
  describe "tagsP" $ do
    it "(1)" $ do
      parse f "" ":hoge:" `shouldBe` Right ["hoge"]
    it "(2)" $ do
      parse f "" ":hoge:foo:" `shouldBe` Right ["hoge", "foo"]
    it "(3)" $ do
      isLeft (parse f "" "") `shouldBe` True
    it "(4)" $ do
      isLeft (parse f "" ":") `shouldBe` True
    it "(5)" $ do
      isLeft (parse f "" "::") `shouldBe` True
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
  describe "titleP" $ do
    let f = (titleP :: Parser (Title Tx.Text))
    it "(1)" $ do
      parse f "" "**** hoge"
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
      parse f "" "*** [10/10] hoge"
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
      parse f "" "*** TODO [10/10] hoge"
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
      parse f "" "*** DONE [10/10] hoge    "
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
      parse f "" "*** DONE [10/10] ho  ge    "
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
      parse f "" "*** DONE [10/10] ho  ge    ::"
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
      parse f "" "*** DONE [10/10] ho  ge    :hoge:"
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
      parse f "" "*** DONE [10/10] ho  ge <2024-12-01 土 12:00>"
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
      parse f "" "*** DONE [10/10] ho ge  <2024-12-01 火 12:00> :hoge:"
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
      parse f "" "*** ho ge  <2024-12-01 木 12:00>"
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
      parse f "" "***** DONE 加入申込書の納品 :片岡:"
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
      parse f "" "***** 石見銀山"
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
    let f = (otherExtremeP :: Parser (Other Tx.Text))
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
      parse f "" "<2025-04-10 月 10:00> CLOSED: <2026-04-01 火>"
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
                <: #geocode @= mempty
                <: nil)
    it "(3)" $ do
      parse f "" "hoge <2025-04-10 月 10:00>"
        `shouldBe`
        Right ( #timestamps @= [ #begin @= makeUTC 2025 4 10 10 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil ]
                <: #others @= "hoge "
                <: #geocode @= mempty
                <: nil)
    it "(4)" $ do
      parse f "" "hoge <2025-04-10 月> foo"
        `shouldBe`
        Right ( #timestamps @= [ #begin @= makeUTC 2025 4 10 0 0
                                 <: #datetype @= Normal
                                 <: #active @= True
                                 <: #end @= Nothing
                                 <: nil ]
                -- <: #others @= ["hoge ", "foo"]
                <: #others @= "hoge foo"
                <: #geocode @= mempty
                <: nil)
    it "(5)" $ do
      parse f "" "hoge <2025-04-10 月> foo [[http://google.co.jp][Google]] buz! SCHEDULED: <2025-04-11 火 12:00-17:00>"
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
                <: #others @=
                   fold [ "hoge "
                        , "foo "
                        , "<a href=\"http://google.co.jp\">Google</a>", "buz! "]
                <: #geocode @= mempty
                <: nil)
    it "(6)" $ do
      parse f "" "hoge <2025-04-10 月> foo [[http://google.co.jp][Google]] buz! SCHEDULED: <2025-04-11 火 12:00-17:00> {{かみあり製麺}{島根県出雲市斐川町学頭1815-1}}"
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
                <: #others @=
                   fold [ "hoge "
                        , "foo "
                        , "<a href=\"http://google.co.jp\">Google</a>", "buz! "]
                <: #geocode @= [GeS "かみあり製麺" (Just "島根県出雲市斐川町学頭1815-1")]
                <: nil)
    it "(7)" $ do
      parse f "" "hoge <2025-04-10 月> foo {{かみあり製麺}{島根県出雲市斐川町学頭1815-1}} [[http://google.co.jp][Google]] buz! SCHEDULED: <2025-04-11 火 12:00-17:00>"
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
                <: #others @= foldMap id
                                      [ "hoge "
                                      , "foo "
                                      , "<a href=\"http://google.co.jp\">Google</a>", "buz! "]
                <: #geocode @= [GeS "かみあり製麺" (Just "島根県出雲市斐川町学頭1815-1")]
                <: nil)
  -- describe "lineParse" $ do
  --   it "(1)" $ do
  --     isLL <$> parse lineParse "" "**** hoge" `shouldBe` Right True
  --     isLL <$> parse lineParse "" " *** hoge" `shouldBe` Right False
  --     isLL <$> parse lineParse "" "*** [10/10] hoge" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** TODO [10/10] hoge" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** DONE [10/10] hoge    " `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** DONE [10/10] ho  ge    " `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** DONE [10/10] ho  ge    ::" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "** DONE [10/10] ho  ge    :hoge:" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** DONE [10/10] ho  ge <2024-12-01 土 12:00>" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** DONE [10/10] ho ge  <2024-12-01 火 12:00> :hoge:" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "*** ho ge  <2024-12-01 木 12:00>" `shouldBe` Right True
  --     isLL <$> parse lineParse "" "***** DONE 加入申込書の納品 :片岡:" `shouldBe` Right True
  --   it "(2)" $ do
  --     parse lineParse "" "***** 石見銀山" `shouldBe`
  --       Right (LL (#label      @= "石見銀山"
  --               <: #level      @= 5
  --               <: #todo       @= Nothing
  --               <: #tags       @= []
  --               <: #timestamps @= []
  --               <: #paragraph  @= mempty
  --               <: #properties @= mempty
  --               <: #location   @= mempty
  --               <: #path       @= mempty
  --               <: nil))
  --     -- parse lineParse "" ":PROPERTIES:" `shouldBe`
  --     --   Right (LP ("PROPERTIES", ""))
  --     parse lineParse "" ":LOCATION: 石見銀山" `shouldBe`
  --       Right (LP ("LOCATION", "石見銀山"))
  --     parse lineParse "" ":END:" `shouldBe`
  --       Right (LP ("END", ""))
  --     parse lineParse "" "<2025-05-03 土 10:00-11:40>" `shouldBe`
  --       Right (LO (#timestamps @= [ #begin @= makeUTC 2025 5 3 10 0
  --                                <: #datetype @= Normal
  --                                <: #active @= True
  --                                <: #end @= (Just $ makeUTC 2025 5 3 11 40)
  --                                <: nil]
  --                  <: #others @= mempty
  --                  <: #geocode @= mempty
  --                  <: nil))

  --     parse lineParse "" "[[https://www.google.com/maps/dir][自宅～石見銀山]]"
  --       `shouldBe`
  --       Right (LO (#timestamps @= mempty
  --                  <: #others @= "<a href=\"https://www.google.com/maps/dir\">自宅～石見銀山</a>"
  --                  <: #geocode @= mempty
  --                  <: nil))
  -- -- it "(2)" $ do
  --   --   parse otherP "" "CLOSED: [2025-03-24 月 11:58] SCHEDULED: <2025-03-24 月>"
  --   --     `shouldBe`
  --   --     Right (#timestamps @= [ #begin       @= makeUTC 2025 3 24 11 58
  --   --                             <: #datetype @= Normal
  --   --                             <: #active   @= False
  --   --                             <: #end      @= Nothing <: nil
  --   --                           , #begin       @= makeUTC 2025 3 24 0 0
  --   --                             <: #datetype @= Normal
  --   --                             <: #active   @= True
  --   --                             <: #end      @= Nothing <: nil]
  --   --            <: #others @= mempty
  --   --            <: #link @= mempty
  --   --            <: nil)

  -- describe "otherP" $ do
  --   let f = otherRefineP
  --   it "(1)" $ do
  --     parse f "" "  <2025-01-01 月 10:00-12:00> CLOSED: <2025-04-01 火 11:00>"
  --       `shouldBe`
  --       Right ( #timestamps @=
  --               [ #begin @= makeUTC 2025 1 1 10 0
  --                 <: #datetype @= Normal
  --                 <: #active   @= True
  --                 <: #end      @= (Just $ makeUTC 2025 1 1 12 0)
  --                 <: nil
  --               , #begin @= makeUTC 2025 4 1 11 0
  --                 <: #datetype @= Closed
  --                 <: #active   @= True
  --                 <: #end      @= Nothing
  --                 <: nil]
  --               <: #others @= "  "
  --               <: #geocode @= mempty
  --               <: nil )
  --   it "(2)" $ do
  --     parse f "" "Haskell Compiler" `shouldBe`
  --       Right ( #timestamps @= []
  --             <: #others @= "Haskell Compiler"
  --             <: #geocode @= mempty
  --             <: nil )
  --   it "(3)" $ do
  --     parse f "" "[[http://www.google.co.jp][Google]]" `shouldBe`
  --       Right ( #timestamps @= []
  --             <: #others @= "<a href=\"http://www.google.co.jp\">Google</a>"
  --             <: #geocode @= mempty
  --             <: nil )
  --   it "(4)" $ do
  --     parse f "" "[[http://www.google.co.jp]]" `shouldBe`
  --       Right ( #timestamps @= []
  --             <: #others @= "<a href=\"http://www.google.co.jp\">http://www.google.co.jp</a>"
  --             <: #geocode @= mempty
  --             <: nil )
  --   it "(5)" $ do
  --     parse f "" "<2025-04-01 土 10:00>" `shouldBe`
  --       Right ( #timestamps @=
  --               [ #begin @= makeUTC 2025 4 1 10 0
  --                 <: #datetype @= Normal
  --                 <: #active @= True
  --                 <: #end @= Nothing
  --                 <: nil]
  --             <: #others @= mempty
  --             <: #geocode @= mempty
  --             <: nil )
  --   it "(6)" $ do
  --     parse f "" "Google [[http://www.google.co.jp][Google]] link"
  --       `shouldBe`
  --       Right ( #timestamps @= []
  --             <: #others @= foldMap id
  --                                   ["Google "
  --                                   , "<a href=\"http://www.google.co.jp\">Google</a>"
  --                                   , "link"]
  --             <: #geocode @= mempty
  --             <: nil )
