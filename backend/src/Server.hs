{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Server (runApp, app) where

import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger
import           System.Environment                   (getEnv)
import qualified Web.Scotty                           as S

app' :: S.ScottyM ()
app' = do
  S.get "/" $ S.file "./public/index.html"
  S.get "/hello" $ S.html "hello"

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = do
  port <- fmap read $ getEnv "PORT"
  S.scotty port $ do
    S.middleware logStdoutDev
    app'
