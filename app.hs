{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req


createRequest :: Req (JsonResponse Object)
createRequest = let payload = object [ "foo" .= (10 :: Int), "bar" .= (20 :: Int)]
                in req
                   POST
                   (https "httpbin.org" /: "post")
                   (ReqBodyJson payload)
                   jsonResponse
                   mempty


main :: IO ()
main = do
  response <- runReq defaultHttpConfig createRequest
  print $ responseBody response

