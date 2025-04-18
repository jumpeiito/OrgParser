{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Org.GoogleDrive.Client
  (
  --   Client (..)
  -- , Oauth (..)
  -- , WithClient
  -- , WithAccessToken
  -- , clientFromFile
  -- , aliveAccessToken
  -- , getRefreshToken
  )
where

import           Control.Monad.Reader
import           Control.Monad.Catch       (MonadThrow)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.KeyMap         (elems)
import           System.Environment        (getEnv)
import           Data.String.Conversions   (convertString)
import qualified Data.Text                 as Tx
import qualified Data.ByteString.Lazy      as B
import           Network.HTTP.Req
import qualified Network.HTTP.Types.URI    as Types
import           Text.URI                  (URI)
import qualified Text.URI                  as URI

type Text = Tx.Text

data InitialJSON = Init
  { clientID       :: Text
  , projectID      :: Text
  , authURI        :: Text
  , tokenURI       :: Text
  , clientSecret   :: Text
  , redirectURI    :: [Text]
  , permissionCode :: Text }
  deriving (Show, Eq)

instance FromJSON InitialJSON where
  parseJSON (Object v) = do
    obj <- v .: "web"
    uri <- obj .: "redirect_uris" >>= parseJSON
    let keys = ["client_id", "project_id", "auth_uri"
               , "token_uri" , "client_secret", "permisson_code"]
    [i, p, a, t, s, pc] <- sequence $ map (obj .:) keys
    return (Init i p a t s uri pc)

driveInitial :: IO FilePath
driveInitial = getEnv "ORG" >>= return . (++ "/driveClient.JSON")

driveInitialJSON :: IO (Maybe InitialJSON)
driveInitialJSON = do
  file <- driveInitial
  bs   <- B.readFile file
  return $ decode bs

makeQuery :: Text -> Text -> Maybe URI.QueryParam
-- makeQuery :: B.ByteString -> B.ByteString -> Maybe URI.QueryParam
makeQuery k v =
  let
    convert = URI.mkQueryValue
              . Tx.replace "%25" "%"
              . convertString
              . Types.urlEncode True
              . convertString
  in
    URI.QueryParam <$> URI.mkQueryKey k <*> convert v

getPermissionURI :: ReaderT InitialJSON IO Text
getPermissionURI = do
  auth     <- asks authURI
  redirect <- asks (head . redirectURI)
  clientid <- asks clientID

  uri      <- liftIO $ URI.mkURI auth
  let param = [ ("scope", "https://www.googleapis.com/auth/drive")
              , ("access_type",   "offline")
              , ("response_type", "code")
              , ("redirect_uri",  redirect)
              , ("client_id",     clientid)]
  let query = case mapM (uncurry makeQuery) param of
        Nothing -> mempty
        Just q  -> q
  return $ URI.render $ uri { URI.uriQuery = query }

driveKeyFile :: IO FilePath
driveKeyFile = getEnv "HOME" >>= return . (++ "/.drivekey")

driveKey :: IO B.ByteString
driveKey = driveKeyFile >>= B.readFile

_test :: IO ()
_test = do
  Just init <- driveInitialJSON
  getRefreshToken `runReaderT` init

getRefreshToken :: ReaderT InitialJSON IO ()
getRefreshToken = do
  redirect <- asks (head . redirectURI)
  clientid <- asks clientID
  clientsc <- asks clientSecret
  permiss  <- asks permissionCode
  tokenuri <- asks tokenURI
  uri      <- liftIO $ URI.mkURI tokenuri
  let params :: [(Text, Text)]
      params = [ ("code",          permiss)
               , ("client_id" ,    clientid)
               , ("client_secret", clientsc)
               , ("redirect_uri",  redirect)
               , ("grant_type",    "authorization_code")
               -- , ("access_type",   "offline")
               ]
      query :: Option scheme
      query   = foldMap (uncurry (=:)) params
      reqHead = "Content-Type" `header` "application/x-www-form-urlencoded"
  -- liftIO $ print redirect
  runReq defaultHttpConfig $ do
    case useHttpsURI uri of
      Nothing -> liftIO $ print ("parseError" :: String)
      Just (url, _) -> do
        res <- req POST url NoReqBody jsonResponse (query <> reqHead)
        liftIO $ print (responseBody res :: Value)
