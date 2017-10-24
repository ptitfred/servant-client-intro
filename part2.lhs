As a prelude, the imports and language extensions required for the code in this
literate haskell file to compile (hopefully)

> {-# LANGUAGE DataKinds         #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeOperators     #-}

> import Data.Aeson
> import Data.Aeson.Types (typeMismatch)
> import Data.Proxy
> import Data.Text        (Text)
> import Servant.API
> import Servant.Client


Let's consider a monitoring API:

> newtype Status = Status { getStatus :: Bool }

> type MonitoringAPI = "api" :> "status" :> Get '[JSON] Status

If you want to consume this API (in Haskell obviously), you just have to define
a function matching inputs and outputs in the right monad, for instance for the
status API:

> checkStatusClient :: ClientM Status

And we let servant-client provide us with an implementation:

> checkStatusClient = client (Proxy :: Proxy MonitoringAPI)

The Proxy is a way to pass MonitoringAPI as a type constraint to client.

---------------------------------------

We then have to evaluate this monad to obtain an IO action:

> checkStatus :: IO (Either ServantError Status)
> checkStatus = getClientEnv >>= runClientM checkStatusClient

> getClientEnv :: IO ClientEnv
> getClientEnv = do
>   manager <- newManager defaultManagerSettings
>   let baseUrl = BaseUrl { baseUrlScheme = Http
>                         , baseUrlHost   = "localhost"
>                         , baseUrlPort   = 3000
>                         , baseUrlPath   = ""
>                         }
>   pure $ ClientEnv manager baseUrl

defaultManagerSettings for http urls, you might want the tls variant for https.

See [1] for details about runClientM.

This won't be enough, we need to define FromJSON Status to consume the response:

> instance FromJSON Status where
>   parseJSON (Object o) = Status <$> o .: "status"
>   parseJSON invalid    = typeMismatch "Status" invalid

=======================================

[1] https://hackage.haskell.org/package/servant-client-0.11/docs/Servant-Client.html#v:runClientM
