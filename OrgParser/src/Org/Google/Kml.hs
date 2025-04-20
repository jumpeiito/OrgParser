{-# LANGUAGE DataKinds #-}
module Org.Google.Kml
  (
    kml
  , toDocument
  , PlaceMark (..)
  )
where

import           Text.XML
import qualified Data.Text       as Tx
import qualified Data.Map.Strict as Map
import           Data.Tagged
import           Data.Proxy

type Text = Tx.Text

data PlaceMark = P
  { name        :: Text
  , description :: Text
  , point       :: (Float, Float) }
  deriving (Show)

pointText :: PlaceMark -> Text
pointText p =
  let (lat, lng) = point p in
  Tx.pack (show lng ++ "," ++ show lat ++ ",0")

toContent :: (PlaceMark -> Text) -> PlaceMark -> Node
toContent f p = NodeContent (f p)

pointElement :: PlaceMark -> Tagged "Point" Element
pointElement p  =
  let
    coordinate = NodeElement
                 $ Element "coordinates" mempty [toContent pointText p]
  in
    (Proxy `tagWith`) $ Element "Point" mempty [coordinate]

nameElement :: PlaceMark -> Tagged "Name" Element
nameElement p =
  (Proxy `tagWith`)
  $ Element "name" mempty [toContent name p]

descriptionElement :: PlaceMark -> Tagged "Description" Element
descriptionElement p =
  (Proxy `tagWith`)
  $ Element "description" mempty [toContent description p]

toElement :: PlaceMark -> Tagged "PlaceMark" Element
toElement p =
  let
    elements ::
      Tagged "Point" Element ->
      Tagged "Name" Element ->
      Tagged "Description" Element -> [Node]
    elements pe ne de =
      let (p', n', d') = (unTagged pe, unTagged ne, unTagged de) in
        map NodeElement [p', n', d']
  in
    (Proxy `tagWith`)
    $ Element "Placemark" mempty
    $ elements (pointElement p) (nameElement p) (descriptionElement p)

toDocument :: [PlaceMark] -> Document
toDocument ps = Document (Prologue mempty Nothing mempty) (kml ps) []

kml :: [PlaceMark] -> Element
kml es = Element "kml" attr [document]
  where
    makeNode n c = NodeElement $ Element n mempty c
    attr = Map.insert "xmlns" "http://www.opengis.net/kml/2.2" mempty
    document = "Document" `makeNode` [name', folder]
    name'    = "name"     `makeNode` [NodeContent "test"]
    folder   = "Folder"   `makeNode` (fname : elements)
    fname    = "name"     `makeNode` [NodeContent "layer1"]
    elements = map (NodeElement . unTagged . toElement) es
