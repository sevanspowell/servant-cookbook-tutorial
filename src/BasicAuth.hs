{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Control.Concurrent
import Control.Exception
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

type Username = T.Text
type Password = T.Text
type Website = T.Text

data User = User
  { user :: Username
  , pass :: Password
  , site :: Website
  } deriving (Eq, Show)

-- Could be a Postgres connection, a file, anything
type UserDB = Map.Map Username User

-- Create a "database" from a list of users
createUserDB :: [User] -> UserDB
createUserDB users = Map.fromList [ (user u, u) | u <- users ]

-- Our test database
userDB :: UserDB
userDB = createUserDB
  [ User "john" "shhh" "john.com"
  , User "foo" "bar" "foobar.net"
  ]

-- API

-- A 'GET /mysite' endpoint, protected by basic authentication
type API = BasicAuth "People's websites" User :> "mysite" :> Get '[JSON] Website

api :: Proxy API
api = Proxy

server :: Server API
server usr = return (site usr)

-- Provided we are given a user database, we can supply a function that checks
-- the basic auth credentials against our database.
--   NOTE: More realistically we would take a database connection or connection
--   pool. 
checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ \basicAuthData ->
  let
    username = decodeUtf8 (basicAuthUsername basicAuthData)
    password = decodeUtf8 (basicAuthPassword basicAuthData)
  in
    case Map.lookup username db of
      Nothing -> pure NoSuchUser
      Just u -> if pass u == password
        then pure (Authorized u)
        else pure BadPassword 

runApp :: UserDB -> IO ()
runApp db = run 8080 (serveWithContext api ctx server)
  where ctx = checkBasicAuth db :. EmptyContext

getSite :: BasicAuthData -> ClientM Website
getSite = client api

clientMain :: IO ()
clientMain = do
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp userDB) killThread $ \_ ->
    runClientM (getSite u) (ClientEnv mgr (BaseUrl Http "localhost" 8080 ""))
      >>= print

  where
    u = BasicAuthData "john" "shhh"
