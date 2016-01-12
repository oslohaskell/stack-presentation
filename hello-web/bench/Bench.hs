module Main where

import Criterion.Main

import Example (fib)

main :: IO ()
main = defaultMain [
    bgroup "fib" [ bench (show n) (whnf fib n) | n <- [1,3..9]]
  ]
