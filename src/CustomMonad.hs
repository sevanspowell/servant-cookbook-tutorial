{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module CustomMonad where

import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           GHC.Generics                (Generic)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Network.Wai.Handler.Warp    (run)

import           Servant
import           Servant.Client

newtype Book = Book String deriving (Show, Generic)
instance ToJSON Book
instance FromJSON Book

type GetBooks = Get '[JSON] [Book]
type AddBook = ReqBody '[JSON] Book :> PostCreated '[JSON] Book
type BooksAPI = "books" :> (GetBooks :<|> AddBook)

api :: Proxy BooksAPI
api = Proxy

data State = State
  { books :: TVar [Book]
  }

type AppM = ReaderT State Handler

server :: ServerT BooksAPI AppM
server = getBooks :<|> addBook
  where
    getBooks :: AppM [Book]
    getBooks = do
      State { books = p } <- ask
      liftIO $ atomically $ readTVar p

    addBook :: Book -> AppM Book
    addBook book = do
      State { books = p } <- ask
      liftIO $ atomically $ readTVar p >>= writeTVar p . (book :)
      pure book

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

clientMain :: IO ()
clientMain = do
  let port = 8080
  mgr <- newManager defaultManagerSettings
  initialBooks <- atomically $ newTVar []
  let runApp = run port $ app $ State initialBooks
  bracket (forkIO runApp) killThread $ \_ -> do
    let getBooksClient :<|> addBookClient = client api
    let printBooks = getBooksClient >>= liftIO . print
    _ <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" port "")) $ do
      _ <- printBooks
      _ <- addBookClient $ Book "Harry Potter and the Order of the Phoenix"
      _ <- printBooks
      _ <- addBookClient $ Book "To Kill a Mockingbird"
      _ <- printBooks
      _ <- addBookClient $ Book "The Picture of Dorian Gray"
      printBooks
    pure ()

