module Github.FakeServer
    ( application
    ) where

import           Github.API     (API)
import           Github.Types

import           Data.Proxy
import           Network.Wai    (Application)
import           Servant.API
import           Servant.Server

application :: Application
application = serve (Proxy :: Proxy API) server

server :: Server API
server = getPullRequest :<|> listPullRequests

getPullRequest :: Maybe UserAgent -> Owner -> Repo -> PullRequestNumber -> Handler PullRequest
getPullRequest = undefined

listPullRequests :: Maybe UserAgent -> Owner -> Repo -> Maybe PullRequestState -> Handler [PullRequest]
listPullRequests = undefined
