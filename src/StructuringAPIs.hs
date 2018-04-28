{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module StructuringAPIs where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant

-- Data
type UserId = Int

data User = User { userName :: String, age :: Int }
  deriving Generic

instance FromJSON User
instance ToJSON User

type ProductId = Int

data Product = Product { productName :: String }
  deriving Generic

instance FromJSON Product
instance ToJSON Product

-- API
type API = FactoringAPI
      :<|> SimpleAPI "users" User UserId
      :<|> SimpleAPI "products" Product ProductId

-- Two endpoints:
--   - GET /x/<some 'Int'>[?y=<some 'Int'>]
--   - POST /x/<some 'Int'>
type FactoringAPI =
  "x" :> Capture "x" Int :>
      (    QueryParam "y" Int :> Get '[JSON] Int
      :<|>                       Post '[JSON] Int
      )

factoringServer :: Server FactoringAPI
factoringServer x = getXY :<|> postX
  where getXY Nothing  = pure x
        getXY (Just y) = pure (x + y)

        postX = return (x - 1)

-- Three endpoints
--   - GET  /<name>
--   - GET  /<name>/<some 'i'>
--   - POST /<name>
type SimpleAPI (name :: Symbol) a i = name :>
  (                         Get  '[JSON] [a]
  :<|> Capture "id" i    :> Get  '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

simpleServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

userServer :: Server (SimpleAPI "users" User UserId)
userServer = simpleServer
  (pure [])
  (\userid -> pure $
    if userid == 0
    then User "john" 64
    else User "everybody else" 10
  )
  (\user -> pure NoContent)

productServer :: Server (SimpleAPI "products" Product ProductId)
productServer = simpleServer
  (pure [])
  (\_productId -> pure $ Product "Great stuff")
  (\_product -> pure NoContent)

api :: Proxy API
api = Proxy

app :: Application
app = serve api $
  factoringServer :<|> userServer :<|> productServer
