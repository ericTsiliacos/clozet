{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Server (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200 'hello'" $ do
      get "/hello" `shouldRespondWith` "hello" {matchStatus = 200}

