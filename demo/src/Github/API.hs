{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Github.API where

import           Github.Types

import           Data.Proxy
import           Servant.API

type API = GetPullRequest :<|> ListPullRequests

api :: Proxy API
api = Proxy

-- https://developer.github.com/v3/pulls/#get-a-single-pull-request
-- GET /repos/:owner/:repo/pulls/:number
type GetPullRequest =
     Header "User-Agent" UserAgent
  :> "repos"
  :> Capture "owner" Owner
  :> Capture "repo" Repo
  :> "pulls"
  :> Capture "number" PullRequestNumber
  :> Get '[JSON] PullRequest

-- https://developer.github.com/v3/pulls/#list-pull-requests
-- GET /repos/:owner/:repo/pulls
type ListPullRequests =
     Header "User-Agent" UserAgent
  :> "repos"
  :> Capture "owner" Owner
  :> Capture "repo" Repo
  :> "pulls"
  :> QueryParam "state" PullRequestState
  :> Get '[JSON] [PullRequest]
