{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Servant.ClientRunner
    ( AuthClientM
    , AuthGet
    , AuthPost
    , AuthPut
    , runClient
    , runAuthClient
    ) where

import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client

type ApiKey = String

type AuthClientM a = Maybe ApiKey -> ClientM a

type AuthGet  a b = QueryParam "key" ApiKey :> Get  a b
type AuthPost a b = QueryParam "key" ApiKey :> Post a b
type AuthPut  a b = QueryParam "key" ApiKey :> Put  a b

runAuthClient :: String -> String -> Bool -> Maybe ApiKey -> AuthClientM a -> IO (Either ServantError a)
runAuthClient h p s k a = runClient h p s (a k)

runClient :: String -> String -> Bool -> ClientM a -> IO (Either ServantError a)
runClient host path https action = do
  let (scheme, port) = if https then (Https, 443) else (Http, 80)
      managerSettings = if https then tlsManagerSettings else defaultManagerSettings
      endpoint = BaseUrl { baseUrlScheme = scheme
                         , baseUrlHost = host
                         , baseUrlPort = port
                         , baseUrlPath = path
                         }
  manager <- newManager managerSettings
  runClientM action (ClientEnv manager endpoint)
