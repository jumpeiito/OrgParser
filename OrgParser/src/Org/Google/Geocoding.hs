{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Org.Google.Geocoding
  (
    Geocode (..)
  , makeKmlFile
  , geocode
  )
where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Lens              ((^.))
import           Network.HTTP.Req
import           Data.Foldable             (toList)
import           Data.Function             ((&))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson.KeyMap         as KeyMap
import           Data.String.Conversions   (convertString)
import qualified Data.Text                 as Tx
import qualified Data.Text.IO              as TxIO
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (catMaybes, fromJust, isNothing)
import           System.Environment        (getEnv)
import           Text.XML

import qualified Org.Parse.Text            as P
import           Org.Conduit               (forGeocode, GeocodeMap)
import           Org.Google.Kml            (toDocument, PlaceMark (..))

data Config = Cfg
  { apiKeyFile :: FilePath
  , requestURL :: Url 'Https }

data Geocode = G
  { formattedAddress :: Maybe Text
  , geometry         :: (Float, Float)
  , placeID          :: Maybe Text }
  deriving (Show, Eq)

type Text = Tx.Text
type App  = ReaderT Config IO

instance FromJSON Geocode where
  parseJSON (Object v) = do
    result  <- (v .: "results") :: Parser Value
    objects <- result & withArray ""
                 (\a -> do
                     objList <- traverse (withObject ".." pure) $ toList a
                     return $ foldr KeyMap.union mempty objList)
    let parseFloat g
          | isNothing g = return (0.0, 0.0)
          | otherwise = do
              loc <- fromJust g .: "location"
              (,) <$> (loc .: "lat") <*> (loc .: "lng")
    G <$> (objects .:? "formatted_address")
      <*> (objects .:? "geometry" >>= parseFloat)
      <*> (objects .:? "place_id")
  parseJSON invalid    =
    prependFailure "parsing Geocode failed, "
    (typeMismatch "Object" invalid)

config :: Config
config = Cfg
  { apiKeyFile = ".geocoder.apikey"
  , requestURL = https "maps.googleapis.com"
                 /: "maps" /: "api" /: "geocode" /: "json" }

apiKey :: App Text
apiKey = do
  basename  <- asks apiKeyFile
  directory <- liftIO $ getEnv "ORG"
  let filename = directory ++ "/" ++ basename
  liftIO $ TxIO.readFile filename

geocode :: Text -> App Geocode
geocode address = do
  url <- asks requestURL
  key <- apiKey
  runReq defaultHttpConfig $ do
    let query = "key" =: key <> "address" =: address
    res <- req GET url NoReqBody jsonResponse query
    return $ responseBody res

geocodeWriteFile :: Document -> IO ()
geocodeWriteFile docs = do
  orgDir <- getEnv "ORG"
  let file = orgDir ++ "/" ++ "map.kml"
  TxIO.writeFile file $ convertString $ renderText def docs

titleLabels :: [P.Title] -> Tx.Text
titleLabels = Tx.intercalate " " . map (Tx.pack . show . P.Geo)

toPlacemarks :: GeocodeMap -> Config -> IO [PlaceMark]
toPlacemarks gmap cfg = do
  let addresses = Map.toList gmap
  ps <- (`runReaderT` cfg) $
    forM addresses $ \(location, titles) -> do
      g <- geocode location
      let desc = titleLabels titles
      if (geometry g == (0.0, 0.0)) || (length titles /= 1)
        then return Nothing
        else return $ Just $ P desc location (geometry g)
  return $ catMaybes ps

makeKmlFile :: IO ()
makeKmlFile = do
  let f t = (t ^. #label == "島根旅行") && (t ^. #level == 3)
  (trueM, falseM) <- forGeocode f
  pts <- toPlacemarks trueM config
  pfs <- toPlacemarks falseM config
  geocodeWriteFile $ toDocument pts pfs
