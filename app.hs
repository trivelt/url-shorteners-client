{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req

main :: IO ()
main = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  r <-
    req
      POST -- method
      (https "httpbin.org" /: "post")
      (ReqBodyJson payload) 
      jsonResponse
      mempty
  liftIO $ print (responseBody r :: Value)
