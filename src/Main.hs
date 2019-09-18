{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Web.Authenticate.OAuth
import           GHC.Generics
import           Data.Aeson
import           Network.HTTP.Client
import qualified Network.HTTP.Client.TLS       as TLS
import           Models
import           Database
import           Configuration.Dotenv           ( loadFile
                                                , defaultConfig
                                                )
import           System.Environment             ( getEnv )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.UTF8           ( fromString )

readEnv :: String -> IO ByteString
readEnv = fmap fromString . getEnv


-- refactor code to use ReaderT
-- to pass the config around different functions.
-- currently explicitly passing around the args.
main :: IO ()
main = do
  loadFile defaultConfig
  ck  <- readEnv "OAUTH_CONSUMER_KEY"
  cs  <- readEnv "OAUTH_CONSUMER_SECRET"
  at  <- readEnv "ACCESS_TOKEN"
  ats <- readEnv "ACCESS_TOKEN_SECRET"

  dbMigration
  est <- twitterTrends (mkOAuth ck cs) (mkCred at ats)
  case est of
    Left  s   -> error s
    Right trs -> mapM_ insertTrend (concatMap trends trs)

mkOAuth :: ByteString -> ByteString -> OAuth
mkOAuth ck cs = newOAuth { oauthServerName     = "api.twitter.com"
                         , oauthConsumerKey    = ck
                         , oauthConsumerSecret = cs
                         }

mkCred :: ByteString -> ByteString -> Credential
mkCred = newCredential

newtype User = User { screen_name :: String } deriving (Show, Generic)

data Status = Status
  {
    text :: !String
  , lang :: !String
  , user :: !User
  } deriving(Show, Generic)

newtype Search = Search { statuses :: [Status]} deriving(Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromJSON Status
instance ToJSON Status
instance FromJSON Search
instance ToJSON Search

newtype SearchString = SearchString String

twitterSearch
  :: OAuth -> Credential -> SearchString -> IO (Either String Search)
twitterSearch auth cred (SearchString term) = do
  req <-
    parseUrlThrow
    $  "https://api.twitter.com/1.1/search/tweets.json?count=100&q="
    ++ term
  signAndGetResponse auth cred req

twitterTrends :: OAuth -> Credential -> IO (Either String [Trends])
twitterTrends auth cred = do
  req <- parseUrlThrow "https://api.twitter.com/1.1/trends/place.json?id=1"
  signAndGetResponse auth cred req

newtype Trends = Trends { trends :: [Trend] } deriving(Show, Generic)

instance ToJSON Trends
instance FromJSON Trends

signAndGetResponse
  :: (FromJSON a) => OAuth -> Credential -> Request -> IO (Either String a)
signAndGetResponse auth cred req = do
  manager   <- newManager TLS.tlsManagerSettings
  signedreq <- signOAuth auth cred req
  res       <- httpLbs signedreq manager
  return $ eitherDecode $ responseBody res
