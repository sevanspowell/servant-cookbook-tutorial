{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Network.Wai.Handler.Warp

import qualified StructuringAPIs (app)

main :: IO ()
main = run 8080 StructuringAPIs.app
