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
import           Control.Monad.Catch       (MonadThrow, catch)
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
  , permissionCode :: Text
  , accessToken    :: Text
  , refreshToken   :: Text }
  deriving (Show, Eq)

data RefreshJSON =
  RJ { rjaToken      :: Text
     , rjExpire      :: Integer
     , rjTokenExpire :: Integer
     , rjScope       :: Text
     , rjTokenType   :: Text }
  deriving (Show)

instance FromJSON InitialJSON where
  parseJSON (Object v) = do
    obj <- v .: "web"
    uri <- obj .: "redirect_uris" >>= parseJSON
    let keys = ["client_id", "project_id", "auth_uri"
               , "token_uri" , "client_secret", "permission_code"
               , "refresh_token", "access_token"]
    [i, p, a, t, s, pc, rt, at] <- sequence $ map (obj .:) keys
    return (Init i p a t s uri pc at rt)

instance ToJSON InitialJSON where
  toJSON (Init cid pid aURI tURI cs rURI pCode aToken rToken) =
    object [ "client_id"       .= cid
           , "project_id"      .= pid
           , "auth_uri"        .= aURI
           , "token_uri"       .= tURI
           , "client_secret"   .= cs
           , "redirect_uri"    .= rURI
           , "permission_code" .= pCode
           , "access_token"    .= aToken
           , "refresh_token"   .= rToken]

instance FromJSON RefreshJSON where
  parseJSON (Object v) = RJ <$> (v .: "access_token")
                            <*> (v .: "expires_in")
                            <*> (v .: "refresh_token_expires_in")
                            <*> (v .: "scope")
                            <*> (v .: "token_type")
  parseJSON invalid    =
    prependFailure "parsing RefreshJSON failed, "
    (typeMismatch "Object" invalid)

googleOauthTokenServer :: Url 'Https
googleOauthTokenServer =
  https "accounts.google.com" /: "o" /: "oauth2" /: "token"

clientWriteFile :: InitialJSON -> IO ()
clientWriteFile c = do
  clientf <- driveInitial
  B.writeFile clientf (encode c)

driveInitial :: IO FilePath
driveInitial = getEnv "ORG" >>= return . (++ "/driveClient.JSON")

driveInitialJSON :: IO (Maybe InitialJSON)
driveInitialJSON = do
  file <- driveInitial
  bs   <- B.readFile file
  return $ decode bs

makeQuery :: Text -> Text -> Maybe URI.QueryParam
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


getRefreshToken :: ReaderT InitialJSON IO ()
getRefreshToken = do
  redirect <- asks (head . redirectURI)
  clientid <- asks clientID
  clientsc <- asks clientSecret
  permiss  <- asks permissionCode
  let params :: [(Text, Text)]
      params = [ ("code",          permiss)
               , ("client_id" ,    clientid)
               , ("client_secret", clientsc)
               , ("redirect_uri",  redirect)
               , ("grant_type",    "authorization_code")
               ]
      query :: Option scheme
      query   = foldMap (uncurry (=:)) params
      reqHead = "Content-Type" `header` "application/x-www-form-urlencoded"
  runReq defaultHttpConfig $ do
    res <- req
           POST
           (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token")
           NoReqBody
           jsonResponse
           (query <> reqHead)
    liftIO $ print (responseBody res :: Value)

refreshAccessToken :: ReaderT InitialJSON IO Text
refreshAccessToken = do
  init <- ask
  cid  <- asks clientID
  csc  <- asks clientSecret
  rt   <- asks refreshToken
  let params =
          [ ("client_id" ,    cid)
          , ("client_secret", csc)
          , ("refresh_token", rt)
          , ("grant_type",    "refresh_token")] :: [(Text, Text)]
  let query = foldMap (uncurry (=:)) params
  runReq defaultHttpConfig $ do
    res <- req POST googleOauthTokenServer NoReqBody lbsResponse query
    case decode (responseBody res) of
      Nothing    -> return mempty
      Just rjson -> do
       let newAccessToken = rjaToken rjson
       liftIO $ clientWriteFile (init { accessToken = newAccessToken })
       return newAccessToken

validateAccessToken :: ReaderT InitialJSON IO Bool
validateAccessToken = do
  atoken <- asks accessToken
  let query = "access_token" =: atoken
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

aliveAccessToken :: ReaderT InitialJSON IO Text
aliveAccessToken = do
  valid <- validateAccessToken
  case valid of
    True  -> asks accessToken
    False -> refreshAccessToken

_test :: IO ()
_test = do
  Just init <- driveInitialJSON
  refreshAccessToken `runReaderT` init >>= print
