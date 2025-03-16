module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Data.String.Strip
import Data.Time

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

mktime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
mktime y mo d h mi = UTCTime (fromGregorian y mo d) (timeToDiffTime (h, mi))

spec :: Spec
spec = do
  describe "strip" $ do
    it "orgTagsParse" $ do
      parse orgTagsParse "" ":hoge:foo:" `shouldBe` Right ["hoge", "foo"]
    it "orgTagsParse2" $ do
      parse orgTagsParse "" "::" `shouldBe` Right []
    it "orgTimeParse" $ do
      parse orgTimeParse "" "12:00" `shouldBe` Right (12, 0)
    it "orgDateYMDParse" $ do
      parse orgDateYMDParse "" "2024-12-01" `shouldBe` Right (2024, 12, 1)
    it "orgDateCoreParse2" $ do
      parse orgDateCoreParse "" "2024-12-01 @" `shouldBe`
        Right (mktime 2024 12 1 0 0)
    it "orgDateParse1--active date" $ do
      parse orgDateParse "" "<2024-12-01 @>" `shouldBe`
        Right (Active $ mktime 2024 12 1 0 0)
    it "orgDateParse2--active date with time" $ do
      parse orgDateParse "" "<2024-01-31 @ 13:58>" `shouldBe`
        Right (Active $ mktime 2024 1 31 13 58)
    it "orgDateParse3--inactive date" $ do
      parse orgDateParse "" "[2025-02-28 @]" `shouldBe`
        Right (InActive $ mktime 2025 2 28 0 0)
    it "orgDateParse4--inactive date with time" $ do
      parse orgDateParse "" "[2025-02-28 @ 23:10]" `shouldBe`
        Right (InActive $ mktime 2025 2 28 23 10)
    it "orgDateParse5--scheduled date" $ do
      parse orgDateParse "" "SCHEDULED: <2025-12-01 @>" `shouldBe`
        Right (Scheduled $ mktime 2025 12 1 0 0)
    it "orgDateParse6--scheduled date with time" $ do
      parse orgDateParse "" "SCHEDULED: <2025-12-01 @ 11:59>" `shouldBe`
        Right (Scheduled $ mktime 2025 12 1 11 59)
    it "orgDateParse7--deadline date" $ do
      parse orgDateParse "" "DEADLINE: <2025-03-01 @>" `shouldBe`
        Right (Deadline $ mktime 2025 3 1 0 0)
    it "orgDateParse8--deadline date with time" $ do
      parse orgDateParse "" "DEADLINE: <2025-03-01 @ 12:01>" `shouldBe`
        Right (Deadline $ mktime 2025 3 1 12 1)
    it "orgDateParse9--range" $ do
      parse orgDateParse "" "<2024-12-01 @> - <2024-12-02 .>" `shouldBe`
        Right (Range (mktime 2024 12 1 0 0, mktime 2024 12 2 0 0))
    it "orgDateParse10--range with time" $ do
      parse orgDateParse "" "<2024-12-01 月 12:00> - <2024-12-02 火 12:00>" `shouldBe`
        Right (Range (mktime 2024 12 1 12 0, mktime 2024 12 2 12 0))
    it "orgTitleParse" $ do
      parse orgTitleParse "" "**** hoge" `shouldBe`
        Right (OrgTitle { title = "hoge", level = 4, todo = Nothing, children = [] })
    it "orgTitleParse2" $ do
      parse orgTitleParse "" "*** [10/10] hoge" `shouldBe`
        Right (OrgTitle { title = "hoge", level = 3, todo = Nothing, children = [] })
    it "orgTitleParse3" $ do
      parse orgTitleParse "" "**** TODO [10/10] hoge" `shouldBe`
        Right (OrgTitle { title = "hoge", level = 4, todo = Just "TODO", children = [] })
    it "orgTitleParse4" $ do
      parse orgTitleParse "" "**** DONE [10/10] hoge" `shouldBe`
        Right (OrgTitle { title = "hoge", level = 4, todo = Just "DONE", children = [] })
    it "orgTitleParse5" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge" `shouldBe`
        Right (OrgTitle { title = "ho ge", level = 3, todo = Just "DONE", children = [] })
    it "orgTitleParse6" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  ::" `shouldBe`
        Right (OrgTitle { title = "ho ge  ", level = 3, todo = Just "DONE", children = [] })
    it "orgTitleParse7" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  :hoge:" `shouldBe`
        Right (OrgTitle { title = "ho ge  ", level = 3, todo = Just "DONE", children = [] })
    it "orgTitleParse8" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  <2024-12-01 @ 12:00>" `shouldBe`
        Right (OrgTitle { title = "ho ge  ", level = 3, todo = Just "DONE", children = [] })
    it "orgTitleParse9" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge  <2024-12-01 @ 12:00> :hoge:" `shouldBe`
        Right (OrgTitle { title = "ho ge  ", level = 3, todo = Just "DONE", children = [] })
    it "orgTitleParse10--unit test" $ do
      let title  = "*** DONE [10/10] ho ge  <2024-12-01 @ 12:00> :hoge:"
          parser = (,,) <$> (orgTitleParse <* many space)
                        <*> (orgDateParse <* many space)
                        <*> orgTagsParse
      parse parser "" title `shouldBe`
        Right (OrgTitle { title = "ho ge  ", level = 3, todo = Just "DONE", children = [] }
              , Active (mktime 2024 12 1 12 0)
              , ["hoge"])
    it "orgPropertyParse" $ do
      parse orgPropertyParse "" ":PROPERTIES:" `shouldBe` Right PropBegin
    it "orgPropertyParse2" $ do
      parse orgPropertyParse "" ":END:" `shouldBe` Right PropEnd
    it "orgPropertyParse3" $ do
      parse orgPropertyParse "" ":LOCATION:  hoge" `shouldBe` Right (PropLocation "hoge")
    it "orgPropertyParse4" $ do
      parse orgPropertyParse "" ":JOINER: ho ge" `shouldBe` Right (PropJoiner "ho ge")
    it "orgPropertyParse5" $ do
      parse orgPropertyParse "" ":CUSTOM_ID: 2 04" `shouldBe`
        Right (PropOther ("CUSTOM_ID", "2 04"))
    it "orgLinkParse1" $ do
      parse orgLinkParse "" "[[http://www.google.co.jp]]" `shouldBe`
        Right (OrgLink "http://www.google.co.jp" Nothing)
    it "orgLinkParse2" $ do
      parse orgLinkParse "" "[[http://www.google.co.jp][Google]]" `shouldBe`
        Right (OrgLink "http://www.google.co.jp" (Just "Google"))
