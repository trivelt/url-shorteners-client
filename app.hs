{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO
import Network.HTTP.Req
import GHC.Generics (Generic)

import Control.Exception
import Control.Monad.Reader
import Control.Monad.IO.Class

import Data.Text
import Data.Aeson
import Data.Hashable
import Data.Bifunctor
import Data.ByteString.Char8 as Char8
import qualified Data.HashMap.Strict as HM


type APIKeys = HM.HashMap ShortenerService Char8.ByteString


data ShortenerService = ShrtLnkDev | Tly | TinyUID deriving (Eq, Generic)
instance Hashable ShortenerService


apiKeysConfig :: APIKeys
apiKeysConfig = HM.fromList [(ShrtLnkDev, "API_KEY")]


getUrl :: IO String
getUrl = System.IO.putStr "URL: " >> hFlush stdout >> System.IO.getLine



shrtlnkDevRequest :: String -> ReaderT APIKeys Req (JsonResponse Object)
shrtlnkDevRequest url = let payload = object [ "url" .= url ]
                 in do
                 config <- ask
                 let api_key = HM.lookupDefault "" ShrtLnkDev config
                 lift $ req
                    POST
                    (https "shrtlnk.dev" /: "api" /: "v2" /: "link")
                    (ReqBodyJson payload)
                    jsonResponse
                    (header "api-key" api_key)


shrtlnkDevResponseHandler :: HttpResponseBody (JsonResponse Object) -> Text
shrtlnkDevResponseHandler r = case HM.lookup "shrtlnk" r of
                            Just (String url) -> url
                            _ -> ""


tlyRequest :: String -> ReaderT APIKeys Req (JsonResponse Object)
tlyRequest url = let payload = object [ "long_url" .= url ]
                 in lift $ req
                    POST
                    (https "t.ly" /: "api" /: "v1" /: "link" /: "shorten")
                    (ReqBodyJson payload)
                    jsonResponse
                    mempty


tlyResponseHandler :: HttpResponseBody (JsonResponse Object) -> Text
tlyResponseHandler r = case HM.lookup "short_url" r of
                            Just (String url) -> url
                            _ -> ""


tinyUIDRequest :: String -> ReaderT APIKeys Req (JsonResponse Object)
tinyUIDRequest url = let payload = object [ "url" .= url ]
                in lift $ req
                   POST
                   (https "tinyuid.com" /: "api" /: "v1" /: "shorten")
                   (ReqBodyJson payload)
                   jsonResponse
                   mempty


tinyUIDResponseHandler :: HttpResponseBody (JsonResponse Object) -> Text
tinyUIDResponseHandler r = case HM.lookup "result_url" r of
                            Just (String url) -> url
                            _ -> ""


runRequest :: APIKeys -> (ShortenerService, ReaderT APIKeys Req b) -> IO (ShortenerService, Either HttpException b)
runRequest env service_and_request = let req = snd service_and_request
                in do
                    let req_with_env = runReaderT req env
                    resp <- try (runReq defaultHttpConfig req_with_env)
                    return (fst service_and_request, resp)


allRequests :: [(ShortenerService, String -> ReaderT APIKeys Req (JsonResponse Object))]
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
    responses <- sequence $ runRequest apiKeysConfig <$> bimap id ($ url) <$> allRequests
    let contents = getShortUrl <$> responses
    mapM_ System.IO.putStrLn contents

