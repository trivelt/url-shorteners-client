{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req


tinyUIDRequest :: String -> Req (JsonResponse Object)
tinyUIDRequest url = let payload = object [ "url" .= url ]
                in req
                   POST
                   (https "tinyuid.com" /: "api" /: "v1" /: "shorten")
                   (ReqBodyJson payload)
                   jsonResponse
                   mempty


main :: IO ()
main = do
  response <- runReq defaultHttpConfig (tinyUIDRequest "https://polydev.pl")
  print $ responseBody response

