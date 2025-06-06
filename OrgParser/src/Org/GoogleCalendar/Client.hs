{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Org.GoogleCalendar.Client
  (
    Client (..)
  , Oauth (..)
  , WithClient
  , WithAccessToken
  , clientFromFile
  , aliveAccessToken
  , getRefreshToken
  )
where

import  Control.Monad.Catch
import  Control.Monad.Reader
import  Data.Maybe              (fromMaybe)
import  Data.Text               (Text)
import  Data.Aeson
import  Data.Aeson.Types
-- import  Data.String.Conversions (convertString)
import  qualified Data.ByteString.Lazy as B
import  Network.HTTP.Req
import  System.Environment

data Client =
  Client { clientID     :: String
         , clientSecret :: String
         , clientOauth  :: Oauth
         , permission   :: String }
  deriving (Show)

data Oauth =
  Oauth { oauthScope   :: String
        , oauthKey     :: String
        , accessToken  :: String
        , refreshToken :: String
        , redirectURI  :: String }
  deriving (Show)

data RefreshJSON =
  RJ { rjaToken      :: String
     , rjExpire      :: Integer
     , rjTokenExpire :: Integer
     , rjScope       :: String
     , rjTokenType   :: String }
  deriving (Show)

testOauth :: Oauth
testOauth = Oauth mempty mempty mempty mempty mempty
testClient :: Client
testClient = Client mempty mempty testOauth mempty

googleOauthTokenServer :: Url 'Https
googleOauthTokenServer =
  https "accounts.google.com" /: "o" /: "oauth2" /: "token"

instance ToJSON Client where
  toJSON (Client id' sec o p) =
    object [ "client_id"       .= id'
           , "client_secret"   .= sec
           , "oauth"           .= toJSON o
           , "permission_code" .= p]

instance ToJSON Oauth where
  toJSON (Oauth scope key atoken rtoken rURI) =
    object [ "scope"         .= scope
           , "key"           .= key
           , "access_token"  .= atoken
           , "refresh_token" .= rtoken
           , "redirect_uri"  .= rURI ]

instance FromJSON Client where
  parseJSON (Object v) = Client <$> (v .: "client_id")
                                <*> (v .: "client_secret")
                                <*> (v .: "oauth")
                                <*> (v .: "permission_code")
  parseJSON invalid    =
    prependFailure "parsing Client failed, "
    (typeMismatch "Object" invalid)

instance FromJSON Oauth where
  parseJSON (Object v) = Oauth <$> (v .: "scope")
                               <*> (v .: "key")
                               <*> (v .: "access_token")
                               <*> (v .: "refresh_token")
                               <*> (v .: "redirect_uri")
  parseJSON invalid    =
    prependFailure "parsing Oauth failed, "
    (typeMismatch "Object" invalid)

instance FromJSON RefreshJSON where
  parseJSON (Object v) = RJ <$> (v .: "access_token")
                            <*> (v .: "expires_in")
                            <*> (v .: "refresh_token_expires_in")
                            <*> (v .: "scope")
                            <*> (v .: "token_type")
  parseJSON invalid    =
    prependFailure "parsing RefreshJSON failed, "
    (typeMismatch "Object" invalid)

type WithClient a = ReaderT Client IO a
type WithAccessToken a = ReaderT (String, Client) IO a

clientFile :: IO FilePath
clientFile = flip (++) "/access.json" <$> getEnv "ORG"

clientFromFile :: IO Client
clientFromFile = do
  clientf    <- clientFile
  bytestring <- B.readFile clientf
  return (testClient `fromMaybe` decode bytestring)

clientWriteFile :: Client -> IO ()
clientWriteFile c = do
  clientf <- clientFile
  B.writeFile clientf (encode c)

validateAccessToken :: WithClient Bool
validateAccessToken = do
  client <- ask
  let opt = [("access_token", accessToken $ clientOauth client)]
  let query = foldMap (uncurry (=:)) opt
  let url = https "oauth2.googleapis.com" /: "tokeninfo" :: Url 'Https
  runReq defaultHttpConfig $ do
    _ <- req GET url NoReqBody lbsResponse query
    return True
  `catch`
    errorHandle
  where
    errorHandle err =
      case err of
        VanillaHttpException _ -> do
          liftIO $ print ("AccessToken to refresh" :: String)
          return False
        JsonHttpException e -> do
          liftIO $ print e; return False

refreshAccessToken :: WithClient String
refreshAccessToken = do
  client <- ask
  let params =
          [ ("client_id" ,    clientID client)
          , ("client_secret", clientSecret client)
          , ("refresh_token", refreshToken $ clientOauth client)
          , ("grant_type",    "refresh_token")] :: [(Text, String)]
  let query = foldMap (uncurry (=:)) params
  runReq defaultHttpConfig $ do
    res <- req POST googleOauthTokenServer NoReqBody lbsResponse query
    case decode (responseBody res) of
      Nothing    -> return mempty
      Just rjson -> do
        let newAccessToken = rjaToken rjson
        let oldo = clientOauth client
        let newo = oldo { accessToken = newAccessToken }
        liftIO $ clientWriteFile (client { clientOauth = newo })
        return newAccessToken

aliveAccessToken :: WithClient String
aliveAccessToken = do
  valid <- validateAccessToken
  case valid of
    True  -> asks $ accessToken . clientOauth
    False -> refreshAccessToken

---- https://note.com/daddysoffice/n/n8505a8ae8e98 この通りにやればできた
---- https://accounts.google.com/o/oauth2/v2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcalendar&access_type=offline&redirect_uri=http%3A%2F%2Flocalhost&response_type=code&client_id= <-ここに入れてブラウザでアクセスすると,認証コードが出る
getRefreshToken :: IO ()
getRefreshToken = do
  config <- clientFromFile
  let params :: [(Text, String)]
      params = [ ("code",          permission config)
               , ("client_id" ,    clientID config)
               , ("client_secret", clientSecret config)
               , ("redirect_uri",  "http://localhost")
               , ("grant_type",    "authorization_code")
               , ("access_type",   "offline")
               ]
      query :: Option scheme
      query  = foldMap (uncurry (=:)) params
  print $ queryParamToList query
  -- runReq defaultHttpConfig $ do
  --   res <- req
  --          POST
  --          -- googleOauthTokenServer
  --          (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token")
  --          NoReqBody
  --          jsonResponse
  --          query
  --   liftIO $ print (responseBody res :: Value)
