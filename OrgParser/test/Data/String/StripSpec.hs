module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Data.String.Strip

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "strip" $ do
    it "orgTagsParse" $ do
      parse orgTagsParse "" ":hoge:foo:" `shouldBe` Right ["hoge", "foo"]
    it "orgTimeParse" $ do
      parse orgTimeParse "" "12:00" `shouldBe` Right (12, 0)
    it "orgDateParse" $ do
      parse orgDateParse "" "2024-12-01" `shouldBe` Right (2024, 12, 1)
    -- it "removes leading and trailing whitespace" $ do
    --   strip "\t  foo bar\n" `shouldBe` "foo bar"
    -- it "is idempotent" $ property $
    --   \str -> strip str === strip (strip str)
