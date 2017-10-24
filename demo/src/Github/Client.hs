module Github.Client
  ( getPullRequest
  , getPullRequests
  , listPullRequests
  ) where

import           Github.API
import           Github.Types
import           Servant.ClientRunner

import           Data.Bifunctor       (first)
import           Servant.API
import           Servant.Client

getPullRequestClient   :: Maybe UserAgent -> Owner -> Repo -> PullRequestNumber      -> ClientM PullRequest
listPullRequestsClient :: Maybe UserAgent -> Owner -> Repo -> Maybe PullRequestState -> ClientM [PullRequest]
getPullRequestClient :<|> listPullRequestsClient = client api

runGithubClient :: ClientM a -> IO (Either String a)
runGithubClient = fmap (first show) . runClient "api.github.com" "" True

getPullRequest :: Owner -> Repo -> PullRequestNumber -> IO (Either String PullRequest)
getPullRequest owner repo nbr = runGithubClient (getPullRequestClient userAgent owner repo nbr)

getPullRequests :: Owner -> Repo -> [PullRequestNumber] -> IO (Either String [PullRequest])
getPullRequests owner repo = runGithubClient . mapM (getPullRequestClient userAgent owner repo)

listPullRequests :: Owner -> Repo -> PullRequestState -> IO (Either String [PullRequest])
listPullRequests owner repo st = runGithubClient $ listPullRequestsClient userAgent owner repo (Just st)

userAgent :: Maybe UserAgent
userAgent = Just "Servant-Client"
