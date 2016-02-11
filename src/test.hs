{-# LANGUAGE OverloadedStrings #-}
module Main where

import TinySrv

index ∷ Route Response
index = do
    host "127.0.0.1"
    emptyPath
    return $ okay "Something Something Index"

main = serve 80 [index]