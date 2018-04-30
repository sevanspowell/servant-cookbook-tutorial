{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Network.Wai.Handler.Warp

import qualified StructuringAPIs (app)
import qualified SQLiteDatabase (clientMain)
import qualified PostgreSQL (clientMain)
import qualified CustomMonad (clientMain)

main :: IO ()
-- main = run 8080 StructuringAPIs.app
-- main = PostgreSQL.clientMain
main = CustomMonad.clientMain
