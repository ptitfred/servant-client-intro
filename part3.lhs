As a prelude, the imports and language extensions required for the code in this
literate haskell file to compile (hopefully)

> {-# LANGUAGE DataKinds         #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeOperators     #-}

> import Data.Aeson
> import Data.Proxy
> import Servant.API
> import Servant.Server
> import qualified Network.Wai.Handler.Warp as Warp

> newtype Status = Status { getStatus :: Bool }

> type MonitoringAPI = "api" :> "status" :> Get '[JSON] Status

> checkStatus :: Handler Status
> checkStatus = pure (Status True)

And you can build a WAI Application for free like this:

> server :: Application
> server = serve (Proxy :: Proxy MonitoringAPI) checkStatus

> main :: IO ()
> main = Warp.run 3000 server

---------------------------------------

But you could do some IO to check something in a database

> checkStatus :: Pool -> Handler Status
> checkStatus = liftIO . doCheckStatus

> doCheckStatus :: Pool -> IO Bool
> doCheckStatus pool = withPool pool doSomeSQL
