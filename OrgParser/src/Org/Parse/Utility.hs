{-# LANGUAGE DataKinds         #-}
module Org.Parse.Utility
  (
    Parser
  , Text
  , fromG
  , makeUTC
  , anyP
  , changeSlots
  )
where

import           GHC.Base               (Alternative)
import           Data.Time
import qualified Data.List              as DL
import           Data.Tagged
import           Data.Void
import qualified Data.Text               as Tx
import           Control.Lens            hiding ((:>), noneOf)
import           Text.Megaparsec

type Parser = Parsec Void Text
type Text   = Tx.Text

fromG ::
  Tagged "Year" Integer ->
  Tagged "Month" Int ->
  Tagged "Day" Int ->
  Day
fromG y m d = fromGregorian (untag y) (untag m) (untag d)

makeUTC ::
  Tagged "Year" Integer ->
  Tagged "Month" Int ->
  Tagged "Day" Int ->
  Tagged "Hour" Int ->
  Tagged "Minute" Int ->
  UTCTime
makeUTC y m d h mi = UTCTime (fromG y m d) dayOfSeconds
  where
    dayOfSeconds =
      secondsToDiffTime $ toInteger h * 3600 + toInteger mi * 60

anyP :: Alternative f => [f a] -> f a
anyP (p:parsers) = DL.foldl' (<|>) p parsers
anyP [] = undefined
{-# INLINE anyP #-}

changeSlots :: ASetter a1 b1 a2 b2 -> b2 -> a1 -> b1
changeSlots sym value tsmp = tsmp & sym .~ value
