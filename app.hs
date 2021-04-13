{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Req
import System.IO

import Control.Monad.IO.Class
import Control.Exception

import Data.Text
import Data.Aeson
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM


data ShortenerService = ShrtLnkDev | Tly | TinyUID


getUrl :: IO String
getUrl = putStr "URL: " >> hFlush stdout >> getLine



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


runRequest :: (ShortenerService, Req a) -> IO (ShortenerService, Either HttpException a)
runRequest service_and_request = let req = snd service_and_request
                                     in do
                                 resp <- try (runReq defaultHttpConfig req)
                                 return (fst service_and_request, resp)


allRequests :: [(ShortenerService, String -> Req (JsonResponse Object))]
allRequests = [(ShrtLnkDev, shrtlnkDevRequest), (Tly, tlyRequest), (TinyUID, tinyUIDRequest)]


getShortUrl :: (ShortenerService, Either HttpException (JsonResponse Object)) -> String
getShortUrl response = case snd response of
    Right x     -> show $ handler $ responseBody x
    Left y      -> "Exception catched: " ++ show y
  where
    handler = case fst response of
          ShrtLnkDev -> shrtlnkDevResponseHandler
          Tly -> tlyResponseHandler
          TinyUID -> tinyUIDResponseHandler


main :: IO ()
main = do
    url <- getUrl
    responses <- sequence $ runRequest <$> bimap id ($ url) <$> allRequests
    let contents = getShortUrl <$> responses
    mapM_ putStrLn contents

