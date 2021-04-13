{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class
import Control.Exception
import Data.Aeson
import Data.Text
import Network.HTTP.Req



shrtlnkDevRequest :: String -> Req (JsonResponse Object)
shrtlnkDevRequest url = let payload = object [ "url" .= url ]
                 in req
                    POST
                    (https "shrtlnk.dev" /: "api" /: "v2" /: "link")
                    (ReqBodyJson payload)
                    jsonResponse
                    (header "api-key" "API_KEY")


shrtlnkDevResponseHandler :: HttpResponseBody (JsonResponse Object) -> Text
shrtlnkDevResponseHandler r = case HM.lookup "shrtlnk" r of
                            Just (String url) -> url
                            _ -> ""


tlyRequest :: String -> Req (JsonResponse Object)
tlyRequest url = let payload = object [ "long_url" .= url ]
                 in req
                    POST
                    (https "t.ly" /: "api" /: "v1" /: "link" /: "shorten")
                    (ReqBodyJson payload)
                    jsonResponse
                    mempty


tlyResponseHandler :: HttpResponseBody (JsonResponse Object) -> Text
tlyResponseHandler r = case HM.lookup "short_url" r of
                            Just (String url) -> url
                            _ -> ""


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


runRequest :: Req a -> IO (Either HttpException a)
runRequest req = try (runReq defaultHttpConfig req)


allRequests :: [Req (JsonResponse Object)]
allRequests = [(shrtlnkDevRequest "http://polydev.pl"), (tlyRequest "http://polydev.pl"), (tinyUIDRequest "http://polydev.pl")]


getContent :: Either HttpException (JsonResponse Object) -> String
getContent response = case response of
    Right x     -> show $ responseBody x
    Left y      -> "Exception catched: " ++ show y


main :: IO ()
main = do
    responses <- sequence $ runRequest <$> allRequests
    let contents = getContent <$> responses
    mapM_ putStrLn contents

