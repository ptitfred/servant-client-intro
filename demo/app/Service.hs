{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Github

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 3000 application
