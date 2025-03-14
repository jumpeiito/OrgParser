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

spec :: Spec
spec = do
  describe "strip" $ do
    it "orgTagsParse" $ do
      parse orgTagsParse "" ":hoge:foo:" `shouldBe` Right ["hoge", "foo"]
      -- parse orgTagsParse "" ":hoge:foo"  `shouldBe` Left ""
    it "orgTimeParse" $ do
      parse orgTimeParse "" "12:00" `shouldBe` Right (12, 0)
    it "orgDateYMDParse" $ do
      parse orgDateYMDParse "" "2024-12-01" `shouldBe` Right (2024, 12, 1)
    it "orgDateCoreParse" $ do
      parse orgDateCoreParse "" "2024-12-01 月 12:00" `shouldBe`
        Right (UTCTime (fromGregorian 2024 12 1) (timeToDiffTime (12, 0)))
    it "orgDateCoreParse2" $ do
      parse orgDateCoreParse "" "2024-12-01 月" `shouldBe`
        Right (UTCTime (fromGregorian 2024 12 1) (timeToDiffTime (0, 0)))
    it "orgTitleParse" $ do
      parse orgTitleParse "" "**** hoge" `shouldBe` Right "hoge"
    it "orgTitleParse2" $ do
      parse orgTitleParse "" "**** [10/10] hoge" `shouldBe` Right "hoge"
    it "orgTitleParse3" $ do
      parse orgTitleParse "" "**** TODO [10/10] hoge" `shouldBe` Right "hoge"
    it "orgTitleParse4" $ do
      parse orgTitleParse "" "**** DONE [10/10] hoge" `shouldBe` Right "hoge"
    it "orgTitleParse5" $ do
      parse orgTitleParse "" "*** DONE [10/10] ho ge" `shouldBe` Right "ho ge"
    -- it "removes leading and trailing whitespace" $ do
    --   strip "\t  foo bar\n" `shouldBe` "foo bar"
    -- it "is idempotent" $ property $
    --   \str -> strip str === strip (strip str)
