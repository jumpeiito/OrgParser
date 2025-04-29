{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Org.Google.Kml
  (
    kml
  , toDocument
  , PlaceMark (..)
  )
where

import           Text.XML
import qualified Data.Text              as Tx
import qualified Data.Map.Strict        as Map
import           Data.Tagged
import           Data.Proxy
import           Data.Time
import qualified Data.List              as Dl
import           Data.Function          (on)

type Text = Tx.Text

data PlaceMark = P
  { name        :: Text
  , description :: Text
  , point       :: (Float, Float)
  , planTime    :: Maybe UTCTime }
  deriving (Show, Eq)

data Folder = F
  { folderName   :: Text
  , nodeElements :: [PlaceMark]
  , selectType   :: Bool }

instance Ord PlaceMark where
  compare = compare `on` planTime

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

toElement :: Bool -> PlaceMark -> Tagged "PlaceMark" Element
toElement isTrue p =
  let
    elementsDefault ::
      Tagged "Point" Element ->
      Tagged "Name" Element ->
      Tagged "Description" Element -> [Node]
    elementsDefault pe ne de =
      let (p', n', d') = (unTagged pe, unTagged ne, unTagged de) in
        map NodeElement [p', n', d']
    elementsOption =
      if isTrue
      then ["styleUrl" `makeNode` [NodeContent "#icon-1899-FFEA00-labelson"]]
      else mempty
    elements = elementsDefault (pointElement p)
                               (nameElement p)
                               (descriptionElement p)
               <> elementsOption
  in
    (Proxy `tagWith`) $ Element "Placemark" mempty elements

toDocument :: [PlaceMark] -> [PlaceMark] -> Document
toDocument pts pfs =
  Document (Prologue mempty Nothing mempty) (kml pts pfs) []

makeNode :: Name -> [Node] -> Node
makeNode n c = NodeElement $ Element n mempty c

iconStyleNode :: Node
iconStyleNode =
  let
    href  = "href"  `makeNode` [NodeContent "https://www.gstatic.com/mapspro/images/stock/503-wht-blank_maps.png"]
    icon  = "Icon"  `makeNode` [href]
    scale = "scale" `makeNode` [NodeContent "1"]
    color = "color" `makeNode` [NodeContent "ff00eaff"]
    hotspotAttr = Map.fromList [ ("x", "32"), ("xunits", "pixels")
                               , ("y", "64"), ("yunits", "insetPixels")]
    hotspot = NodeElement $ Element "hotspot" hotspotAttr mempty
    iconstyle = "IconStyle" `makeNode` [color, scale, icon, hotspot]
    attrs = Map.fromList [("id", "icon-1899-FFEA00-labelson")]
  in
    NodeElement $ Element "Style" attrs [iconstyle]

folderToElements :: Folder -> Node
folderToElements folder = "Folder" `makeNode` (fname : elements)
  where
    fname    = "name" `makeNode` [NodeContent (folderName folder)]
    toNodeElement = NodeElement . unTagged . toElement (selectType folder)
    elements = map toNodeElement (nodeElements folder)

kml :: [PlaceMark] -> [PlaceMark] -> Element
kml pts pfs = Element "kml" attr [document]
  where
    attr     = Map.insert "xmlns" "http://www.opengis.net/kml/2.2" mempty
    document = "Document" `makeNode` [iconStyleNode, name', folder1, folder2]
    name'    = "name"     `makeNode` [NodeContent "test"]
    folder1  = folderToElements $ F "layer1" (Dl.sort pts) True
    folder2  = folderToElements $ F "layer2" (Dl.sort pfs) False
