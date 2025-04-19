{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Org.Google.Geocoding
  (
    Geocode (..)
  , geocode
  )
where

import           Control.Monad.Reader
import           Network.HTTP.Req
import           Data.Foldable             (toList)
import           Data.Function             ((&))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson.KeyMap         as KeyMap
import qualified Data.Text                 as Tx
import qualified Data.Text.IO              as TxIO
import qualified Data.Map.Strict           as Map
import           Data.Functor              ((<&>))
import           System.Environment        (getEnv)
import qualified Data.ByteString.Lazy      as B

import           Org.Conduit               (forGeocode)


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
    result    <- (v .: "results") :: Parser Value
    objects   <- result & withArray ""
                 (\a -> do
                     objList <- traverse (withObject ".." pure) $ toList a
                     return $ foldr (KeyMap.union) mempty objList)
    let geo obj = do
          bool <- obj .:? "geometry"
          case bool of
            Nothing  -> return (0.0, 0.0)
            Just g   -> do
              loc <- g .: "location"
              ((,) <$> (loc .: "lat") <*> (loc .: "lng"))
    G <$> (objects .:? "formatted_address")
      <*> (geo objects)
      <*> (objects .:? "place_id")

config :: Config
config = Cfg
  { apiKeyFile = ".geocoder.apikey"
  , requestURL = https "maps.googleapis.com"
                 /: "maps" /: "api" /: "geocode" /: "json"}

apiKey :: App Text
apiKey = do
  basename  <- asks apiKeyFile
  directory <- liftIO $ getEnv "ORG"
  let filename = directory ++ "/" ++ basename
  liftIO $ TxIO.readFile filename

geocode :: Text -> App ()
geocode address = do
  url <- asks requestURL
  key <- apiKey
  runReq defaultHttpConfig $ do
    let query = "key" =: key <> "address" =: address
    res <- req
           GET
           url
           NoReqBody
           jsonResponse
           query
    liftIO $ print (responseBody res :: Geocode)

_test :: IO ()
_test = do
  addresses <- Map.keys <$> forGeocode
  (`runReaderT` config) $ do
    -- geocode "京都市"
    mapM_ (\a -> do { liftIO $ TxIO.putStrLn a; geocode a }) addresses

_test2 :: IO ()
_test2 = do
  g <- decode <$> B.readFile "c:/users/jumpei/Documents/home/Haskell/OrgParser/OrgParser/src/Org/Google/test.json" :: IO (Maybe Geocode)
  print g

