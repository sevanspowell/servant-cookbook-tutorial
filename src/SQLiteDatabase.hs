{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SQLiteDatabase where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

type Message = String

type API = ReqBody '[PlainText] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

server :: FilePath -> Server API
server dbfile = postMessage :<|> getMessages
  where
    postMessage :: Message -> Handler NoContent
    postMessage msg = do
      liftIO . withConnection dbfile $ \conn ->
        execute conn
                "INSERT INTO messages VALUES (?)"
                (Only msg)
      pure NoContent

    getMessages :: Handler [Message]
    getMessages = fmap (map fromOnly) . liftIO $
      withConnection dbfile $ \conn ->
        query_ conn "SELECT msg FROM messages"

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (serve api $ server dbfile)

postMsg :: Message -> ClientM NoContent
getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

clientMain :: IO ()
clientMain = do
  let dbfile = "test.db"
  initDB dbfile
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp dbfile) killThread $ \_ -> do
    ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      postMsg "hello"
      postMsg "world"
      getMsgs
    print ms
