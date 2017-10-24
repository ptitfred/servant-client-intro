As a prelude, the imports and language extensions required for the code in this
literate haskell file to compile (hopefully)

> {-# LANGUAGE DataKinds     #-}
> {-# LANGUAGE TypeOperators #-}

> import Data.ByteString
> import Data.Text
> import Servant.API


Here is an example of a Servant API:

> newtype Status = Status { getStatus :: Bool }

> type API = "api" :> "status" :> Get '[JSON] Status

:> to build a HTTP url

'[JSON] to list returned encoding.

---------------------------------------

Some endpoints (REST APIs for instance) embed some parameter in the url:

> newtype ResourceId = ResourceId Int
> type Resource = ()
> type API = "api" :> "search" :> Capture "resource-id" ResourceId :> Get '[JSON] [Resource]

GET /api/search/:resource-id


or query parameters:

> type API = "api" :> "search" :> QueryParam "resource-id" ResourceId :> Get '[JSON] [Resource] 

GET /api/search?resource-id=<integer>

---------------------------------------

You can build a more complex API by adding new endpoints and concatenate them:

> data Session = Session
> data User = User { login    :: Text
>                  , password :: ByteString
>                  }

> type API = "api" :> "status"                        :> Get  '[JSON] Status
>       :<|> "api" :> "login" :> ReqBody '[JSON] User :> Post '[JSON] Session

---------------------------------------

You can merge enpoints under a common path:

> type API    = "api" :> (Status :<|> Login)

> type Status = "status"                        :> Get  '[JSON] Status
> type Login  = "login" :> ReqBody '[JSON] User :> Post '[JSON] Session


This is especially useful to version APIs:

> type API = "api" :> ("v1" :> V1 :<|> "v2" :> V2)
> type V1 = ...
> type V2 = ...

But we just describes an API there. Let's reboot to write something useful.
