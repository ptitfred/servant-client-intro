{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Github

import           Data.Semigroup ((<>))
import qualified Data.Text.IO   as T

main :: IO ()
main = do
  putStrLn "Get #800"
  getPullRequest "haskell-servant" "servant" 800 >>= inspect

  putStrLn "Get #797, #800, #803"
  getPullRequests "haskell-servant" "servant" [797, 800, 803] >>= inspectMany

  putStrLn "List Open PRs"
  listPullRequests "haskell-servant" "servant" Open >>= inspectMany

inspect :: Either String PullRequest -> IO ()
inspect = either putStrLn describe

inspectMany :: Either String [PullRequest] -> IO ()
inspectMany = either putStrLn (mapM_ describe)

describe :: PullRequest -> IO ()
describe PullRequest{..} = do
  putStr $ " #" <> show number <> " "
  T.putStrLn $ title <> " by @" <> login author
