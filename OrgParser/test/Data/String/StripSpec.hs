module Data.String.StripSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import Data.String.Strip
import Data.Time

spec :: Spec
spec = do
  describe "normal" $ do
    it "" $ do 2 `shouldBe` 2
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
