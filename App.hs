{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module App where

import System.IO
import Network.HTTP.Req
import Control.Exception
import GHC.Generics (Generic)

import Data.Text
import Data.Aeson
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM

data ShortenerServiceApiConfig =
  ShortenerServiceApiConfig { name          :: Text
                            , apiUrl        :: Text
                            , shortUrlLabel :: Text
                            , longUrlLabel  :: Text
                            , apiKey        :: Maybe Text
                            } deriving (Show, Generic)

instance FromJSON ShortenerServiceApiConfig
instance ToJSON ShortenerServiceApiConfig


getUrl :: IO String
getUrl = System.IO.putStr "URL: " >> hFlush stdout >> System.IO.getLine


parseUrl :: Text -> Url 'Https
parseUrl url = Prelude.foldl (\a b -> a /: b) host otherParts
  where
    parts = splitOn "/" url
    host = https $ Prelude.head parts
    otherParts = Prelude.drop 1 parts


jsonFile :: FilePath
jsonFile = "shorteners.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


loadApiConfigs :: IO [ShortenerServiceApiConfig]
loadApiConfigs = do
    config <- (eitherDecode <$> getJSON) :: IO (Either String [ShortenerServiceApiConfig])
    case config of
        Left _ -> return []
        Right x -> return x


createRequest :: String -> ShortenerServiceApiConfig -> Req (JsonResponse Object)
createRequest url config = req
        POST
        (parseUrl $ apiUrl config)
        (ReqBodyJson payload)
        jsonResponse
        headerObject
    where
        payload = object [ longUrlLabel config .= url ]
        headerObject = case apiKey config of
            Just key -> header "api-key" (encodeUtf8 key)
            Nothing  -> mempty


runRequest :: Req a -> IO (Either HttpException a)
runRequest request =  do
        resp <- try (runReq defaultHttpConfig request)
        return resp


getShortUrl :: ShortenerServiceApiConfig -> Either HttpException (JsonResponse Object) -> String
getShortUrl config response = case response of
    Right x     -> show $ handler $ responseBody x
    Left y      -> "Exception catched: " ++ show y
  where
    handler x = case HM.lookup (shortUrlLabel config) x of
        Just (String url) -> url
        Nothing -> ""


shortenUrl :: String -> ShortenerServiceApiConfig -> IO String
shortenUrl url config = do
    let request = createRequest url config
    response <- runRequest request
    return $ getShortUrl config response



main :: IO ()
main = do
    url <- getUrl
    apiConfigs <- loadApiConfigs
    contents <- sequence $ (shortenUrl url) <$> apiConfigs
    mapM_ System.IO.putStrLn contents

