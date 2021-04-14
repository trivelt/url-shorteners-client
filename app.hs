{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

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
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Types.URI
import Data.Text.Encoding


type APIKeys = HM.HashMap ShortenerService Char8.ByteString


data ShortenerService = ShrtLnkDev | Tly | TinyUID deriving (Eq, Generic)
instance Hashable ShortenerService


apiKeysConfig :: APIKeys
apiKeysConfig = HM.fromList [(ShrtLnkDev, "API_KEY")]


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

data ShortenerServiceApiConfig =
  ShortenerServiceApiConfig { name          :: Text
                            , apiUrl        :: Text
                            , shortUrlLabel :: Text
                            , longUrlLabel  :: Text
                            , apiKey        :: Maybe Text
                            } deriving (Show, Generic)

instance FromJSON ShortenerServiceApiConfig
instance ToJSON ShortenerServiceApiConfig

loadApiConfig :: IO [ShortenerServiceApiConfig]
loadApiConfig = do
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


runRequest :: (ShortenerServiceApiConfig, Req a) -> IO (ShortenerServiceApiConfig, Either HttpException a)
runRequest config_and_request = let req = snd config_and_request
    in do
        resp <- try (runReq defaultHttpConfig req)
        return (fst config_and_request, resp)


getShortUrl :: (ShortenerServiceApiConfig, Either HttpException (JsonResponse Object)) -> String
getShortUrl (config, response) = case response of
    Right x     -> show $ handler $ responseBody x
    Left y      -> "Exception catched: " ++ show y
  where
    handler x = case HM.lookup (shortUrlLabel config) x of
        Just (String url) -> url
        Nothing -> ""


main :: IO ()
main = do
    url <- getUrl
    apiConfigs <- loadApiConfig
    let requests = (\config -> (config, (createRequest url config))) <$> apiConfigs
    responses <- sequence $ runRequest <$> requests
    let contents = getShortUrl <$> responses
    mapM_ System.IO.putStrLn contents

