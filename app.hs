{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text
import Network.HTTP.Req


tinyUIDRequest :: String -> Req (JsonResponse Object)
tinyUIDRequest url = let payload = object [ "url" .= url ]
                in req
                   POST
                   (https "tinyuid.com" /: "api" /: "v1" /: "shorten")
                   (ReqBodyJson payload)
                   jsonResponse
                   mempty


tinyUIDResponseHandler :: HttpResponseBody (JsonResponse Object) -> Text
tinyUIDResponseHandler r = case HM.lookup "result_url" r of
                            Just (String url) -> url
                            _ -> ""


main :: IO ()
main = do
    response <- runReq defaultHttpConfig (tinyUIDRequest "https://polydev.pl")
    let body = responseBody response
    let url = tinyUIDResponseHandler body
    print url

