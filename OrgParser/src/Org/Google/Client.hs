{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Org.Google.Client
  (
    Client (..)
  , RefreshJSON (..)
  , Config (..)
  , AppCore
  , App
  , configDef
  , configCalendar
  , getPermissionURI
  , getRefreshToken
  , aliveAccessToken
  , appCoreCalendar
  )
where

import           Control.Monad           (guard)
import           Control.Monad.Catch     (catch)
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy    as B
import           Data.Functor            ((<&>))
import           Data.Maybe              (fromJust, fromMaybe, isJust)
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Tx
import qualified Data.Text.IO            as TxIO
import           Network.HTTP.Req
import qualified Network.HTTP.Types.URI  as Types
import           System.Environment      (getEnv)
import qualified Text.URI                as URI

type Text = Tx.Text

data Client = Init
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

data Config = Cfg
  { basename         :: String
  , oauthTokenServer :: Url 'Https
  , validateServer   :: Url 'Https
  , getRefreshTokenServer
                     :: Url 'Https
  , scope            :: Text }
  deriving (Show, Eq)

type AppCore = (Config, Client)
type App     = StateT AppCore IO

instance FromJSON Client where
  parseJSON (Object v) = do
    uri <- v .: "redirect_uri" >>= parseJSON
    let keys = ["client_id", "project_id", "auth_uri"
               , "token_uri" , "client_secret", "permission_code"
               , "refresh_token", "access_token"]
    [i, p, a, t, s, pc, rt, at] <- mapM (v .:) keys
    return (Init i p a t s uri pc at rt)
  parseJSON invalid    =
    prependFailure "parsing Geocode failed, "
    (typeMismatch "Object" invalid)

instance ToJSON Client where
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

configDef :: Config -- for Google Drive
configDef = Cfg
  { basename = "driveClient.json"
  , oauthTokenServer = https "accounts.google.com" /: "o" /: "oauth2" /: "token"
  , validateServer = https "oauth2.googleapis.com" /: "tokeninfo"
  , scope = "https://www.googleapis.com/auth/drive"
  , getRefreshTokenServer = https "www.googleapis.com" /: "oauth2" /: "v4" /: "token"
  }

configCalendar :: Config -- for Google Calendar
configCalendar = configDef
  { basename = "calendarClient.json"
  , scope = "https://www.googleapis.com/auth/calendar" }

writeClient :: App ()
writeClient = do
  (cfg, cl) <- get
  cFile     <- liftIO $ clientFile cfg
  liftIO $ B.writeFile cFile (encode cl)

clientFile :: Config -> IO FilePath
clientFile cfg = do
  getEnv "ORG" <&> (++ "/" ++ basename cfg)

client :: Config -> IO (Maybe Client)
client cfg = (clientFile cfg >>= B.readFile) <&> decode

appCore :: Config -> IO (Config, Client)
appCore cfg = do
  c     <- client cfg
  guard $ isJust c
  let client' = fromJust c
  aToken <- aliveAccessToken `evalStateT` (cfg, client')
  return (cfg, client' { accessToken = aToken })

appCoreCalendar :: IO (Config, Client)
appCoreCalendar = appCore configCalendar

makeQuery :: Text -> Text -> Maybe URI.QueryParam
makeQuery k v =
  let
    convert = URI.mkQueryValue
              -- . Tx.replace "%25" "%"
              . convertString
              . Types.urlEncode True
              . convertString
  in
    URI.QueryParam <$> URI.mkQueryKey k <*> convert v

getPermissionURI :: App Text
getPermissionURI = do
  (cfg, cl) <- get
  case redirectURI cl of
    []         -> error "redirectURI error"
    redirect:_ -> do
      let scope'     = scope cfg
      let auth       = authURI cl
      let clientid   = clientID cl
      uri      <- liftIO $ URI.mkURI auth
      let param = [ ("scope", scope')
                  , ("access_type",   "offline")
                  , ("response_type", "code")
                  , ("redirect_uri",  redirect)
                  , ("client_id",     clientid)]
      let query = mempty `fromMaybe` mapM (uncurry makeQuery) param
      return $ Tx.replace "%25" "%"
             $ URI.render
             $ uri { URI.uriQuery = query }

_test :: Config -> IO ()
_test config = do
  cl <- client config
  case cl of
    Nothing -> return ()
    Just c  -> do
      -- txt <- aliveAccessToken `evalStateT` (config, c)
      -- txt <- getPermissionURI `evalStateT` (config, c)
      -- print txt
      getRefreshToken `evalStateT` (config, c)

getRefreshToken :: App ()
getRefreshToken = do
  (cfg, cl) <- get
  let redirect = head $ redirectURI cl
  let params :: [(Text, Text)]
      query :: Option scheme
      params = [ ("code",          permissionCode cl)
               , ("client_id" ,    clientID cl)
               , ("client_secret", clientSecret cl)
               , ("redirect_uri",  redirect)
               , ("grant_type",    "authorization_code")]
      query   = foldMap (uncurry (=:)) params
      reqHead = "Content-Type" `header` "application/x-www-form-urlencoded"
  runReq defaultHttpConfig $ do
    res <- req
           POST
           (getRefreshTokenServer cfg)
           NoReqBody
           jsonResponse
           (query <> reqHead)
    liftIO $ print (responseBody res :: Value)

refreshAccessTokenParse :: Maybe RefreshJSON -> App Text
refreshAccessTokenParse Nothing = return mempty
refreshAccessTokenParse (Just rj) = do
  (cfg, cl) <- get
  let newAccessToken = rjaToken rj
  put (cfg, cl { accessToken = newAccessToken })
  writeClient
  return newAccessToken

refreshAccessToken :: App Text
refreshAccessToken = do
  (cfg, cl) <- get
  let params =
          [ ("client_id" ,    clientID cl)
          , ("client_secret", clientSecret cl)
          , ("refresh_token", refreshToken cl)
          , ("grant_type",    "refresh_token")] :: [(Text, Text)]
  let request = req POST
                    (oauthTokenServer cfg)
                    NoReqBody
                    lbsResponse
                    (foldMap (uncurry (=:)) params)
  res <- (decode . responseBody <$> runReq defaultHttpConfig request)
         `catch` errorHandle
  refreshAccessTokenParse res
  where
    errorHandle = \case
      VanillaHttpException e -> do
        getPermissionURI >>= liftIO . TxIO.putStrLn
        liftIO $ print e
        error "Refresh-Token has expired, and Browse the below urls with Internet Blowserto get the permisson code."
      _ -> do
        -- liftIO $ print e
        return Nothing

validateAccessToken :: App Bool
validateAccessToken = do
  (cfg, cl) <- get
  let atoken = accessToken cl
  let query  = "access_token" =: atoken
  let url    = validateServer cfg
  runReq defaultHttpConfig $ do
    _ <- req GET url NoReqBody lbsResponse query
    return True
  `catch`
    errorHandle
  where
    errorHandle = \case
      VanillaHttpException _ -> do
        liftIO $ print ("AccessToken to refresh" :: String)
        return False
      JsonHttpException e -> do
        liftIO $ print e
        return False

aliveAccessToken :: App Text
aliveAccessToken = do
  valid <- validateAccessToken
  case valid of
    True  -> gets (accessToken . snd)
    False -> refreshAccessToken
