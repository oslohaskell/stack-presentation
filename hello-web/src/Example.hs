{-# LANGUAGE OverloadedStrings #-}

module Example (runApp, app, fib) where

import Data.Aeson (object, (.=))
import Network.Wai (Application)
import Web.Scotty

fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = fib' m
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib' (n-1) + fib' (n-2)

app' :: ScottyM ()
app' = do
  get "/" $ do
    text "hello"
  get "/fib/:number" $ do
    n <- param "number"
    json (object ["input" .= n, "result" .= fib n])


app :: IO Application
app = scottyApp app'

runApp :: IO ()
runApp = scotty 8080 app'
