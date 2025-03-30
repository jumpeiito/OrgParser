module Org.ParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Org.Parse
import Data.Time

main :: IO ()
main = hspec spec

-- mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
-- mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))
defTitle = ParserTitle { title      = mempty
                       , level      = 0
                       , todo       = Nothing
                       , tags       = ParserTags []
                       , timestamps = []}

spec :: Spec
spec = do
  describe "strip" $ do
    it "orgTagsParse" $ do
      parse orgTagsParse "" ":hoge:foo:" `shouldBe` Right (ParserTags ["hoge", "foo"])
    -- it "orgTagsParse2" $ do
    --   parse orgTagsParse "" "::" `shouldBe` Left "parse error"
    it "orgTimeParse" $ do
      parse orgTimeParse "" "12:00" `shouldBe` Right (12, 0)
    it "orgDateYMDParse" $ do
      parse orgDateYMDParse "" "2024-12-01" `shouldBe` Right (2024, 12, 1)
    it "orgDateCoreParse2" $ do
      parse orgDateCoreParse "" "2024-12-01 @" `shouldBe`
        Right (mktime 2024 12 1 0 0)
    it "orgTimeStampParse1--active date" $ do
      parse orgTimeStampParse "" "<2024-12-01 @>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2024 12 1 0 0
                               , datetype = Normal
                               , active   = True
                               , end      = Nothing})
    it "orgTimeStampParse2--active date with time" $ do
      parse orgTimeStampParse "" "<2024-01-31 @ 13:58>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2024 1 31 13 58
                               , datetype = Normal
                               , active   = True
                               , end      = Nothing})
    it "orgTimeStampParse3--inactive date" $ do
      parse orgTimeStampParse "" "[2025-02-28 @]" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2025 2 28 0 0
                               , datetype = Normal
                               , active   = False
                               , end      = Nothing})
    it "orgTimeStampParse4--inactive date with time" $ do
      parse orgTimeStampParse "" "[2025-02-28 @ 23:10]" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2025 2 28 23 10
                               , datetype = Normal
                               , active   = False
                               , end      = Nothing})
    it "orgTimeStampParse5--scheduled date" $ do
      parse orgTimeStampParse "" "SCHEDULED: <2025-12-01 @>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2025 12 1 0 0
                               , datetype = Scheduled
                               , active   = True
                               , end      = Nothing})
    it "orgTimeStampParse6--scheduled date with time" $ do
      parse orgTimeStampParse "" "SCHEDULED: <2025-12-01 @ 11:59>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2025 12 1 11 59
                               , datetype = Scheduled
                               , active   = True
                               , end      = Nothing})
    it "orgTimeStampParse7--deadline date" $ do
      parse orgTimeStampParse "" "DEADLINE: <2025-03-01 @>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2025 3 1 0 0
                               , datetype = Deadline
                               , active   = True
                               , end      = Nothing})
    it "orgTimeStampParse8--deadline date with time" $ do
      parse orgTimeStampParse "" "DEADLINE: <2025-03-01 @ 12:01>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2025 3 1 12 1
                               , datetype = Deadline
                               , active   = True
                               , end      = Nothing})
    it "orgTimeStampParse9--range" $ do
      parse orgTimeStampParse "" "<2024-12-01 @> - <2024-12-02 .>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2024 12 1 0 0
                               , datetype = Normal
                               , active   = True
                               , end      = Just (mktime 2024 12 2 0 0)})
    it "orgTimeStampParse10--range with time" $ do
      parse orgTimeStampParse "" "<2024-12-01 月 12:00> - <2024-12-02 火 12:00>" `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2024 12 1 12 0
                               , datetype = Normal
                               , active   = True
                               , end      = Just (mktime 2024 12 2 12 0)})
    it "orgTimeStampParse11--range with time" $ do
      parse orgTimeStampParse "" "SCHEDULED: <2024-12-01 月 12:00> - <2024-12-02 火 12:00>"
        `shouldBe`
        Right (ParserTimeStamp { begin    = mktime 2024 12 1 12 0
                               , datetype = Scheduled
                               , active   = True
                               , end      = Just (mktime 2024 12 2 12 0)})
    it "orgTitleParse" $ do
      parse orgTitleParse "" "**** hoge" `shouldBe`
        Right (defTitle { title = "hoge" , level = 4 })
    it "orgTitleParse2" $ do
      parse orgTitleParse "" "*** [10/10] hoge" `shouldBe`
        Right (defTitle { title = "hoge" , level = 3 })
    it "orgTitleParse3" $ do
      parse orgTitleParse "" "**** TODO [10/10] hoge" `shouldBe`
        Right (defTitle { title = "hoge" , level = 4 , todo = Just "TODO" })
    it "orgTitleParse4" $ do
      parse orgTitleParse "" "**** DONE [10/10] hoge" `shouldBe`
        Right (defTitle { title = "hoge" , level = 4 , todo = Just "DONE" })
    it "orgTitleParse5" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge" `shouldBe`
        Right (defTitle { title = "ho ge", level = 3, todo = Just "DONE" })
    it "orgTitleParse6" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  ::" `shouldBe`
        Right (defTitle { title = "ho ge  ::", level = 3, todo = Just "DONE" })
    it "orgTitleParse7" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  :hoge:" `shouldBe`
        Right (defTitle { title = "ho ge  "
                        , level = 3
                        , todo  = Just "DONE"
                        , tags  = ParserTags ["hoge"] })
    it "orgTitleParse8" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  <2024-12-01 @ 12:00>" `shouldBe`
        Right (defTitle { title      = "ho ge  "
                        , level      = 3
                        , todo       = Just "DONE"
                        , timestamps =
                          [ParserTimeStamp { begin    = mktime 2024 12 1 12 0
                                           , datetype = Normal
                                           , active   = True
                                           , end      = Nothing}] })
    it "orgTitleParse9" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  <2024-12-01 @ 12:00> :hoge:" `shouldBe`
        Right (defTitle { title      = "ho ge  "
                        , level      = 3
                        , todo       = Just "DONE"
                        , tags       = ParserTags ["hoge"]
                        , timestamps =
                          [ParserTimeStamp { begin    = mktime 2024 12 1 12 0
                                           , datetype = Normal
                                           , active   = True
                                           , end      = Nothing}]})
    it "orgTitleParse10" $ do
      parse orgTitleParse "" "*** ho ge  <2024-12-01 @ 12:00>" `shouldBe`
        Right (defTitle { title      = "ho ge  "
                        , level      = 3
                        , todo       = Nothing
                        , timestamps =
                          [ParserTimeStamp { begin    = mktime 2024 12 1 12 0
                                           , datetype = Normal
                                           , active   = True
                                           , end      = Nothing}] })
    it "orgTitleParse11" $ do
      parse orgTitleParse "" "***** DONE 加入申込書の納品 :片岡:" `shouldBe`
        Right (defTitle { title  = "加入申込書の納品 "
                        , level  = 5
                        , todo   = Just "DONE"
                        , tags   = ParserTags ["片岡"]})
    it "orgTitleLineCoreParse1" $ do
      let title  = "***** DONE 加入申込書の納品 :片岡:"
      parse orgTitleLineCoreParse "" title `shouldBe`
        Right [ defTitle { title  = "加入申込書の納品 "
                         , level  = 5
                         , todo   = Just "DONE"
                         , tags   = ParserTags ["片岡"]} ]
    it "orgTitleLineCoreParse2" $ do
      let title  = "***** DONE 加入申込書の納品 <2025-03-01 @> :片岡:"
      parse orgTitleLineCoreParse "" title `shouldBe`
        Right [ defTitle { title  = "加入申込書の納品 "
                         , level  = 5
                         , todo   = Just "DONE"
                         , tags   = ParserTags ["片岡"]
                         , timestamps =
                           [ParserTimeStamp { begin = mktime 2025 3 1 0 0
                                            , datetype = Normal
                                            , active = True
                                            , end = Nothing }]}]
    -- it "orgPropertyParse" $ do
    --   parse orgPropertyParse "" ":PROPERTIES:" `shouldBe` Right OrgPropertyBegin
    -- it "orgPropertyParse2" $ do
    --   parse orgPropertyParse "" ":END:" `shouldBe` Right OrgPropertyEnd
    -- it "orgPropertyParse3" $ do
    --   parse orgPropertyParse "" ":LOCATION:  hoge" `shouldBe`
    --     Right (OrgProperty ("LOCATION", "hoge"))
    -- it "orgPropertyParse4" $ do
    --   parse orgPropertyParse "" ":JOINER: ho ge" `shouldBe`
    --     Right (OrgProperty ("JOINER", "ho ge"))
    -- it "orgPropertyParse5" $ do
    --   parse orgPropertyParse "" ":CUSTOM_ID: 2 04" `shouldBe`
    --     Right (OrgProperty ("CUSTOM_ID", "2 04"))
    -- it "orgLinkParse1" $ do
    --   parse orgLinkParse "" "[[http://www.google.co.jp]]" `shouldBe`
    --     Right (OrgLink "http://www.google.co.jp" Nothing)
    -- it "orgLinkParse2" $ do
    --   parse orgLinkParse "" "[[http://www.google.co.jp][Google]]" `shouldBe`
    --     Right (OrgLink "http://www.google.co.jp" (Just "Google"))
    -- it "orgTitleLineCoreParse1" $ do
    --   parse orgTitleLineCoreParse "" "** Org Test 1 <2024-12-22 @> :buz:boo:" `shouldBe`
    --     Right [ OrgTitle { title = "Org Test 1 ", level = 2, todo = Nothing, children = []}
    --           , OrgTimeStamp { begin = mktime 2024 12 22 0 0, end = Nothing, datetype = Normal, active = True}
    --           , OrgTags ["buz", "boo"]]
    -- it "orgOtherLineCoreParse1" $ do
    --   let str = " other <2024-12-12 @> foo SCHEDULED: <2024-12-21 @ 13:00>"
    --   parse orgOtherLineCoreParse "" str `shouldBe`
    --     Right [ OrgOther " other "
    --           , OrgTimeStamp { begin = mktime 2024 12 12 0 0, end = Nothing, datetype = Normal, active = True}
    --           , OrgOther " foo "
    --           , OrgTimeStamp { begin = mktime 2024 12 21 13 0, end = Nothing, datetype = Scheduled, active = True}]
