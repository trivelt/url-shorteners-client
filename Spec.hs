{-# LANGUAGE OverloadedStrings #-}
module Spec where
import App
import Network.HTTP.Req
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "app" $ do
    describe "jsonFile" $ do
      it "should return file path" $
        jsonFile `shouldBe` "shorteners.json"

    describe "parseUrl" $ do
        it "should parse Text to Url object" $
            parseUrl "polydev.pl/api/v2/test"
            `shouldBe`
            (https "polydev.pl") /: "api" /: "v2" /: "test"

