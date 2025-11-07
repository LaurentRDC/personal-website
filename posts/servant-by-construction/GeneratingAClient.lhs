---
title: "Generating a client for an API - Servant by construction part 4"
date: 2025-11-06
summary: In this post, we replicate Servant's ability to automatically derive client functions for an API, using plain Haskell.
tags: haskell, web, software engineering, servant by construction
---

This file is a Literate Haskell module, so we must import things before we get going:

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- Suppress some warnings which take away from the beauty
-- of the post
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module GeneratingAClient where

import ApiAsType
    ( (:>),
      (:<|>)(..),
      Capture,
      Get,
      Post,
      QueryParam,
      ReqBody,
      Temperature
    )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Time (Day, fromGregorian)
import GHC.TypeLits (symbolVal, KnownSymbol)
import Network.HTTP.Client qualified as HTTP
import ServingAnApi (API)
\end{code}

<hr>

Here we are. This is the moment I have been writing for, the end-game. After the last post, where we learned how to [serve an API](/posts/servant-by-construction/ServingAnApi.html) while having the compiler enforce the specs of said API, we are ready to generate client functions to query that same API.

The first time I saw this in action, I felt a combination of confusion and awe. In memory of this moment, I'll just start by the end. Behold:

\begin{code}
(forecastLastUpdated :<|> forecastAtDate) :<|> (getTemperature :<|> postTemperature) = client (Proxy :: Proxy API)
\end{code}

where `forecastLastUpdated`, `forecastAtDate`, `getTemperature`, and `postTemperature` are normal Haskell functions that allow us to interact with any server serving the API whose specification respects `API`, which, as a reminder, is:

```haskell
type ForecastAPI =
        "forecast"
            :> ( "lastupdated" :> Get UTCTime
            :<|> Capture "date" Day :> "temperature" :> Get Temperature
               )

type WeatherAPI =
    "weather"
        :> "temperature"
            :> ( QueryParam "city" City :> Get Temperature
            :<|> Capture "city" City :> ReqBody Temperature :> Post
               )

type API = ForecastAPI :<|> WeatherAPI
```

- "Big deal bud, ever heard of OpenAPI?"

OpenAPI is _nice_, but, it's also not enough. It's hard to encode some invariants in an OpenAPI spec. In the set of languages that have [an OpenAPI generator](https://openapi-generator.tech/docs/generators), the level of type sophistication ranges from "huh, types?" to Haskell. OpenAPI sacrifices some of the possible nuances by being more broadly usable.


So, how does this all work? Let's find out.

<hr>

What does one need, in order to interact with an HTTP API? It needs to know:

* What's the hostname, port number, and possibly path where the server can be accessed;
* What the endpoints are;
* What _shape_ does the requests and responses have, for a given endpoint.

We'll tackle the first point first. When interacting with the API, some knowledge must come externally. We'll store this information in its own environment type, `ClientEnv`:

\begin{code}
data ClientEnv = ClientEnv
               { hostName :: String              -- ^ E.g. http://api.server.com
               , portNumber :: Int               -- ^ E.g. 80
               , basePath :: String              -- ^ E.g. /v1
               , networkManager :: HTTP.Manager  -- ^ To make network calls
               }
\end{code}

To promote re-use, we'll package this data into a monad type that had `ClientEnv` as a static environment:

\begin{code}
type ClientM a = ReaderT ClientEnv IO a

runClientM :: ClientEnv -> ClientM a -> IO a
runClientM = flip runReaderT
\end{code}

Now, if you recall the previous post in this series, we know what "shape" should the client functions be, and by shape, I mean the types. So, it's going to be relatively straightward to say that, for example, the `forecastLastUpdated` client function should look like:

\begin{code}
forecastAtDate :: Day -> ClientM Temperature
\end{code}

But, unlike serving the API, we'd like to also define the _logic_ of the client function. Recall that a client function is basically assembles a request to be sent; we'll chase that thought. Consider a very simple request type like so:

\begin{code}

data Req = Req
         { reqBody   :: Maybe String       -- ^ Optional body of the request
         , reqParams :: [(String, String)] -- ^ Key-value pairs of query parameters
         , reqVerb   :: ByteString         -- ^ "GET" or "POST". If unset, the request is not valid.
         , reqPath   :: [String]           -- ^ Path to the endpoint from the base path
				                                   --   of the API, e.g. `["forecast", "2025-12-01", "temperature"]`
         }
\end{code}

Note that I'm being quite sloppy here. For example, we should make `reqVerb` take a sum type rather than any `ByteString`, but I'm trying to keep things as simple as possible.

We'll walk through endpoints and construct client functions that, well, construct `Req`s. Then, we can combine a `Req` with the `ClientEnv` to send a HTTP request. Let's go!

<hr>

As we have done three times already, we will express the functionality we want to add to the `API` type with a typeclass:

\begin{code}
class HasClient api where
    type Client api

    clientWithRoute :: Proxy api -> Req -> Client api
\end{code}

Before we start writing instances, we'll make some helper functions to make it easier to deal with `Req`. First, the empty request:

\begin{code}
emptyReq :: Req
emptyReq = Req {reqBody = Nothing, reqParams = [], reqVerb = mempty, reqPath = []}
\end{code}

We need a way to set the body of the request. Recall that for simplicity, we assume that requests are all plaintext encoded, and so we serialize Haskell datatypes over the wire using the `Show` typeclass:

\begin{code}
setBody :: Show a => a -> Req -> Req
setBody body req = req{reqBody = Just (show body)}
\end{code}

We also need to be able to set the verb of the request -- actually, requests are invalid without a verb! You'll see later how we enforce that all `Req`s that get sent off have a valid verb later:


\begin{code}
setVerb :: ByteString -> Req -> Req
setVerb verb req = req{reqVerb = verb}
\end{code}

We also need to add query parameters:

\begin{code}
addQueryParam :: Show a => String -> a -> Req -> Req
addQueryParam key val req = req {reqParams = reqParams req <> [(key, show val)]}
\end{code}

and we need to be able to extend the path of the endpoint to which the `Req` is sent:

\begin{code}
addToPath :: String -> Req -> Req
addToPath segment req = req{reqPath = reqPath req <> [segment]}
\end{code}

Finally, we also need a method for sending the request, and receiving the payload. We'll re-use a low-level library for this, `http-client`:

\begin{code}
toRequest :: Req -> ClientEnv -> HTTP.Request
toRequest (Req body params verb path) (ClientEnv hostname portnumber basepath _)
    = HTTP.defaultRequest { HTTP.method = verb
                          , HTTP.host = BS.pack hostname
                          , HTTP.port = portnumber
                          , HTTP.path = BS.pack $ basepath <> intercalate "/" path
                          , HTTP.queryString = BS.pack
                                             $ intercalate "&" [ key <> "=" <> val
                                                               | (key, val) <- params
                                                               ]

                          , HTTP.requestBody = HTTP.RequestBodyBS (maybe mempty BS.pack body)
                          }


runRequestNoBody :: Req -> ClientM ()
runRequestNoBody req = do
    clientEnv@(ClientEnv _ _ _ networkManager) <- ask
    liftIO (HTTP.httpNoBody (toRequest req clientEnv) networkManager) <&> HTTP.responseBody

runRequest :: Read a => Req -> ClientM a
runRequest req = do
    clientEnv@(ClientEnv _ _ _ networkManager) <- ask
    liftIO (HTTP.httpLbs (toRequest req clientEnv) networkManager)
        <&> HTTP.responseBody
        <&> BS.toStrict
        <&> BS.unpack
        <&> read
\end{code}

With these helper functions out-of-the-way, we can proceed with the real meat of this post. We start with terminal verbs. Recall that, for simplicity, we assume that all POST responses do not carry a body; hence, the client function must return `()`:

\begin{code}
instance HasClient Post where
    type Client Post = ClientM ()

    clientWithRoute _ req = runRequestNoBody (req & setVerb "POST")
\end{code}

Side note: I love `&` and `<&>` and I wish to see it more. Moving on to GET:

\begin{code}
instance Read a => HasClient (Get a) where
    type Client (Get a) = ClientM a

    clientWithRoute _ req = runRequest (req & setVerb "GET")
\end{code}

`Get` is a little more interesting because we have to specify a return type. Given our API specification, the parsing of the response body should never fail due to an unexpected type.

Let's do `ReqBody` now. A client function for an endpoint using `ReqBody a` should _supply_ the request with a body of type `a`; this means adding an argument to the client function, like we did for server handlers:

```haskell
instance Show a => HasClient (ReqBody a :> sub) where
    type Client (ReqBody a :> sub) = a -> Client sub

    clientWithRoute _ req = ...
```

... wait, what should we write for `clientWithRoute`? Well, in this instance, `clientWithRoute :: Proxy ... -> Req -> Client (ReqBody a :> sub)`, which is

```haskell
clientWithRoute :: Proxy (ReqBody a :> sub)
                -> Req
                -> (a -> Client sub)
```

Since `ReqBody` never appears at the end of an endpoint definition, we must recur with `clientWithRoute`. This also requires us to make sure that the sub-API, `sub`, is an instance of `HasClient`:

\begin{code}
instance (HasClient sub, Show a) => HasClient (ReqBody a :> sub) where
    type Client (ReqBody a :> sub) = a -> Client sub

    clientWithRoute _ req body = clientWithRoute (Proxy :: Proxy sub) (setBody body req)
\end{code}

Great! Let's do `QueryParam`. Recall that in our implementation, a query parameter is always optional. Recall that in `QueryParam name a`, `name` is a type-level string which we can bring to the values world by using `symbolVal`:

\begin{code}
instance (HasClient sub, KnownSymbol name, Show a) => HasClient (QueryParam name a :> sub) where
    type Client (QueryParam name a :> sub) = Maybe a -> Client sub

    clientWithRoute _ req param =
        clientWithRoute
            (Proxy :: Proxy sub)
            (addQueryParam
                (symbolVal (Proxy :: Proxy name))
                param
                req
            )
\end{code}

After this, a `Capture` is going to be easy:

\begin{code}
instance (HasClient sub, Show a) => HasClient (Capture name a :> sub) where
    type Client (Capture name a :> sub) = a -> Client sub

    clientWithRoute _ req capture =
        clientWithRoute
            (Proxy :: Proxy sub)
            (addToPath (show capture) req)
\end{code}

We're almost done. Path segments only add the the path of the request, but otherwise do not add a parameter to our client function:

\begin{code}
instance (HasClient sub, KnownSymbol segment) => HasClient (segment :> sub) where
    type Client (segment :> sub) = Client sub

    clientWithRoute _ req
        = clientWithRoute
            (Proxy :: Proxy sub)
            (addToPath (symbolVal (Proxy :: Proxy segment)) req)
\end{code}

Finally, the ever-interesting `(:<|>)` instance. In this case, no thought is required, since there's only one thing that will type-check:

\begin{code}
instance (HasClient left, HasClient right) => HasClient (left :<|> right) where
    type Client (left :<|> right) = Client left :<|> Client right

    clientWithRoute _ req
        =    clientWithRoute (Proxy :: Proxy left) req
        :<|> clientWithRoute (Proxy :: Proxy right) req
\end{code}

Not that much work! How do we connect `HasClient` with our first demonstration of the function `client`? Behold:

\begin{code}
client :: HasClient api => Proxy api -> Client api
client proxy = clientWithRoute proxy emptyReq
\end{code}

In action, here's how we can fetch the forecasted temperature on Christmas:

\begin{code}
run :: IO ()
run = do
    networkManager <- HTTP.newManager HTTP.defaultManagerSettings
    let clientEnv = ClientEnv
                  { hostName = "myserver.com"
                  , portNumber = 80
                  , basePath = "/api"
                  , networkManager = networkManager
                  }

        christmasDay = fromGregorian 2025 12 25

    runClientM clientEnv (forecastAtDate christmasDay) >>= liftIO . print
\end{code}

`forecastAtDate` is just a regular function, but there's a lot going on under the hood!

<hr>

Automatically generating client functions for an API is beautiful. It looks like magic, but when we look under the hood, it's """just""" type families, partially-applied functions, `Proxy`, and recursion.

Unlike the previous posts, the code above is NOT a highly simplified version of what Servant provides for real clients via [`servant-client`](https://hackage.haskell.org/package/servant-client). The only thing we're missing here is support for the real Servant combinators (powering authentication, streaming, HTTP headers, etc), and allowing for other base monads than `IO`. That's not bad!