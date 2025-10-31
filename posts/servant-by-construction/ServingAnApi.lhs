---
title: "Serving an API - Servant by construction part 3"
date: 2025-10-30
summary: In this post, we replicate Servant's ability to enforce the types of handlers, and automatically route requests, based on a type-level API specification.  
tags: haskell, web, software engineering, servant by construction
---

In the [previous post](/posts/servant-by-construction/TypeSafeLinks.html) in this [series](/posts/servant-by-construction-introduction.html), we derived type-safe links for API endpoints. While this wasn't a revolutionary use of a computer, it set the stage for better, *bolder* things. We learned about extending Servant using typeclasses, and the central role of [`Proxy`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Proxy.html#t:Proxy).

Today, we do some _real_ web development stuff: ~~we'll go too far building abstractions~~ we'll serve an API.

<hr>

This file is a Literal Haskell module, so we need to get some imports out-of-the-way.

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module ServingAnApi where

import ApiAsType
    ( (:>),
      (:<|>)(..),
      Capture,
      City,
      Get,
      Post,
      QueryParam,
      ReqBody,
      Temperature (MkTemperature),
    )
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Time (UTCTime, Day)
import Data.Time qualified
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.Wai.Internal (Request(..), Response(..))
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp qualified as Warp
import Network.HTTP.Types qualified as HTTP
\end{code}

So, recall from the first post in this series, that we have an API we have modelled:

\begin{code}
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
\end{code}

This API, represented by the `API` type, has 4 endpoints, split into two sub-APIs, one for forecasting (`ForecastAPI`) and one for real-time weather measurements (`WeatherAPI`). We want to write a server application to serve this API.

Now, life is short, priorities already changed twice this sprint, and I don't like debugging production issues, so we'll add one more requirement: we want to enforce, at compile-time, that our server application respects our API specification. Let's go!

Our API specification has 4 endpoints. So, our server application should be composed of 4 functions somehow; each function is the *handler* for a request to an endpoint. If you forget the details of our API and label the endpoints `a`, `b`, `c`, and `d`, we need four handler functions `ha`, `hb`, `hc`, and `hd` for endpoints `a`, `b`, `c`, and `d` respectively.

This relationship between an endpoint and its handler function isn't the kind of loving, nurturing relationship you witness on Love Island: it's entirely dictated by whatever the endpoint specification says. What I mean is that there's only one handler function that respects any given endpoint specification. We'll encode this relationship from endpoint to handler function as a *function*.

Wait a minute, I hear you say. The endpoint specification is a type, and a handler function also has a type. How can this be encoded in a function? Well, the de-facto standard Haskell compiler, GHC, has a feature called [*type families*](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/exts/type_families.html), which can be used to model type-level functions. I have written about type families before[^typed-strategy-features] [^dataframes]; I'll assume that you have an understanding of how they work.

[^typed-strategy-features]: [Trading strategies with typed features using Haskell and type families](/posts/typesafe-tradingstrats.html)

[^dataframes]: [Modeling dataframes in Haskell using higher-kinded types](/posts/HKTGenerics.lhs)

So, just like in our previous post, we will build up a result type (in this case, the type of a server) by walking over the structure of an API type. As you may remember, case-by-case analysis on types can be represented by a _typeclass_ in Haskell. I posit that the following typeclass does it:

\begin{code}
class HasServer a where
    type Server a
\end{code}

That is, for each component type of our API (like `Get` and `Capture`), we will have an associated type `Server a`, the type of the server for the part of the API which is `a`. It looks like I'm being loose with language here, but I am not: an endpoint has a handler function, but a whole API has a server. It just so happens that if the API is a single endpoint, then the server is also just a handler function.

As before, we will recursively build the `Server` type for our `API` type, `Server API`, by composing the `Server` for our components. We start with the base case, the return type of an endpoint which is given by the terminal verbs:

\begin{code}
instance HasServer Post where
    type Server Post = IO ()
\end{code}

This is pretty straightforward: in our implementation, for simplicity, the `Post` terminal verb never returns any data. Therefore, the handler function for a `Post` should return nothing, labeled as `()`.
Note that we make our server implementation run in the `IO` monad; this is more realistic, as it allows us to do some side effects like interact with a database.

Let's move on to `Get`. The handler function for a `Get` endpoint returns data:

```haskell
instance HasServer (Get a) where
    type Server (Get a) = IO a
```

Sike, this won't compile. Haskell is _so_ flexible that the compiler can't know if `a` is a type (which can be returned by a function), or something higher-kinded (which _cannot_ be returned by a function). For example, what if `a` = `Proxy`? That's not a type -- `Proxy` is partially-applied, so-to-speak! Therefore, we must be more specific by saying that `a` must be a concrete type. Behold:

\begin{code}
instance HasServer (Get (a :: Type)) where
    type Server (Get a) = IO a
\end{code}

I sure hope that's the last technicality we encounter!

Moving on to other component types, you'll notice that none can be in the end position of an endpoint. This is important: it means that all further definitions of `Server` will recur!

Let's move on to `ReqBody`:

```haskell
instance HasServer sub => HasServer (ReqBody (a :: Type) :> sub) where
    type Server (ReqBody a :> sub) = ...
```

We have to think about this. Clearly, the left hand side must involve `a` and `Server sub` somehow.
We compose API components using `(:>)` or `(:<|>)`. We composed the implementations of `toLink` using recursive function calls. How are we supposed to compose the types here? Since the resulting type of `Server` should be a handler function, we should probably compose `Server (ReqBody a)` and `Server sub` such that they form a function. Well, the composition of types into a function, in Haskell, is done with `(->)`!

\begin{code}
instance HasServer (ReqBody (a :: Type) :> sub) where
    type Server (ReqBody a :> sub) = a -> Server sub
\end{code}

This makes intuitive sense: the handler function for an endpoint which has a request body of type `a`, should take `a` as an _argument_. Right on!

We move to `QueryParam`. Recall that we defined our query parameters to be _optional_, which means that our handler function must take in `Maybe a` as an argument, not `a` directly:

\begin{code}
instance HasServer (QueryParam name (a :: Type) :> sub) where
    type Server (QueryParam name a :> sub) = Maybe a -> Server sub
\end{code}

Great, I'm getting used to this. Now, let's do path segments. That's easy, since they don't affect the logic of our handler function, only the routing of requests (which we will get to later in this post):

\begin{code}
instance KnownSymbol segment => HasServer (segment :> sub) where
    type Server (segment :> sub) = Server sub
\end{code}

Now we do `Capture`. Just like `ReqBody`, this is required and should be a function argument. Therefore:

\begin{code}
instance HasServer (Capture name (a :: Type) :> sub) where
    type Server (Capture name a :> sub) = a -> Server sub
\end{code}

Finally, we have `(:<|>)`. This is the composition of two handler functions, or two subservers, for two parts of an API, into a true _server_. We'll keep this simple for now, and you'll see how we use it in practice:

\begin{code}
instance HasServer (a :<|> b) where
    type Server (a :<|> b) = Server a :<|> Server b
\end{code}

We did all this work... can we serve that API yet?! Don't be so hasty. Look at the scrollbar, we're barely halfway. We need to deal with one more piece of the puzzle: routing requests to the right handler.

<hr>

To route requests, we need to understand the high-level structure of a server. A web server is a program which looks like `Request -> IO Response`

We re-use the Haskell [Web Application Interface](https://hackage-content.haskell.org/package/wai-3.2.4) types `Request` and `Response`, like Servant and many other Haskell web frameworks also do. Now, the question is: which handler function gets the `Request`, and thus provides the `Response`? We need to introduce a method to say whether a handler function matched a route (and thus should process the request), or if the route does not match, in which case another handler function must be tried. We do so using as sum type:

\begin{code}
data RouteResult a
    = Matched a
    | NotMatched
\end{code}

Now, for every part of our API, we want to determine a function of type `Request -> IO (RouteResult Response)`. We will look through the API for the first handler function which returns a `Matched Response`!

As we've done twice now with `HasLink` and `HasServer`, we package the functionality in a class, `HasRoute`:

\begin{code}
type RoutingApplication = Request -> IO (RouteResult Response)

class HasRoute api where
    route :: Proxy api -> Server api -> RoutingApplication
\end{code}

Recall that we need to use `Proxy` here because otherwise the type inference won't work. Yes, even if `Server api` appears in the type signature, because `Server api` is a *type function* whose result is probably not related directly to `api`!

The plan is to look at the path of the request, and recursively match its content to find the right part of the API for which to return `Matched`. As we've done twice now, we start with the terminal verbs:

\begin{code}
buildResponse :: Show a => a -> Response
buildResponse = responseLBS HTTP.status200 mempty . ByteString.fromStrict . Text.encodeUtf8 . Text.pack . show

instance Show a => HasRoute (Get (a :: Type)) where
    route (Proxy :: Proxy (Get a)) (handler :: IO a) = \request ->
        if (requestMethod request == "GET" && null (pathInfo request))
            then Matched . buildResponse <$> handler
            else pure NotMatched
\end{code}

Oof. A couple of things to note. The first is that, as you can see by our definition of `buildResponse`, we are modeling a *very simple* server: a server that never fails (I wish), and thus always returns HTTP200 status codes if a handler is found. Second, the payload of all responses is serialized from the `Show` instance. This is all in the name of simplicity. In a subsequent post, I *may* show you how Servant deals with content types -- send me an e-mail if you'd like to read that!

The logic of this instance isn't particularly complex: if the request is a GET request, and we've consumed all of its path (and therefore it is the empty list, hence `null` returns `True`), then our handler function has indeed matched this route, and we return the result of `action` as `Matched <the response>`. I have annotated `handler` with the type; recall that `Server (Get a) === IO a`! Otherwise, this handler is NOT the right handler for this request.

The instance for `Post` is basically similar, but the response is empty (again, to keep things simple). I'll stop writing out `Proxy` everywhere, and replace it by `_`:

\begin{code}
instance HasRoute Post where
    route _ (handler :: IO ()) = \request ->
        if (requestMethod request == "POST" && null (pathInfo request))
            then Matched . buildResponse <$> handler
            else pure NotMatched
\end{code}

Let's move on to `ReqBody`. This is the first case where we will recur:

\begin{code}
readReqBody :: Read a => Request -> IO a
readReqBody req = read . Text.unpack . Text.decodeUtf8Lenient . ByteString.toStrict <$> strictRequestBody req

instance (Read a, HasRoute sub) => HasRoute (ReqBody (a :: Type) :> sub) where
    route _ handler = \request -> do
        readReqBody request >>=
            \body -> route (Proxy :: Proxy sub) (handler body) request
\end{code}

There's a lot going on here! First, we read the body of the request using `readReqBody`, assuming it is plaintext and always succeeds (insert sarcastic comment). We then pass the *parsed* body down the routing tree, where the body is used by the handler. One subtle, but awesome, thing to see here is that `handler` may be a function with lots of arguments -- results of query parameters and capture parameters -- so `handler body` could be a partially-applied function!

Time for `QueryParam`, which are always optional. Just like `ReqBody`, we don't bother parsing the data properly, and use `read` for simplicity:

\begin{code}
readQueryParam :: Read a => String -> HTTP.Query -> Maybe a
readQueryParam paramName query =
    let paramNameBytes = ByteString.Char8.pack paramName
    in lookup paramNameBytes query
        >>= fmap (read . ByteString.Char8.unpack)

instance (KnownSymbol name, Read a, HasRoute sub) => HasRoute (QueryParam name (a :: Type) :> sub) where
    route _ handler = \request ->
        let (queryParam :: Maybe a) = readQueryParam (symbolVal (Proxy :: Proxy name)) (queryString request)
        in route (Proxy :: Proxy sub) (handler queryParam) request
\end{code}

Recall from the last post, that we can use `symbolVal` to extract the string associated with a type-level string (in this case, `name`). That `name` is used to lookup the value of the appropriate query parameter stored in the `HTTP.Query`, *if* it is found. Just like with `ReqBody`, the parsed value, `queryParam`, is passed to the handler, which may be a partially-applied function!

Onto path segments. As you can imagine, they contribute to *routing* but handlers are unmodified. A path segment which matches the begining of the request path will continue to route, while if the path segment doesn't match, we return `NotMatched`. The key to recursive behavior here is to remove the prefix of the request path on successful match, such that the next segment only has to match the prefix of the path.

\begin{code}
instance (KnownSymbol segment, HasRoute sub) => HasRoute (segment :> sub) where
    route _ handler = \request ->
        let apiSegment = Text.pack $ symbolVal  (Proxy :: Proxy segment)
        in case pathInfo request of
            [] -> pure NotMatched
            (nextSegment:rest) ->
                if  nextSegment == apiSegment
                    then route (Proxy :: Proxy sub)
                           handler
                           (request{pathInfo = rest})
                    else pure NotMatched
\end{code}

Now path captures. We're not going to re-invent the wheel here, using the `Read` instance on the path, and assuming it always succeeds:

\begin{code}
readCapture :: Read a => String -> a
readCapture = read

instance (Read a, HasRoute sub) => HasRoute (Capture name (a :: Type) :> sub) where
    route _ handler = \request -> case pathInfo request of
        [] -> pure NotMatched
        (value:rest) ->
            let captureParam = readCapture (Text.unpack value)
            in route (Proxy :: Proxy sub)
                     (handler captureParam)
                     (request{pathInfo = rest})
\end{code}

Finally, we have `(:<|>)`. This is relatively straightforward: we try the left branch, and return the result of that handler if it returns `Matched`. Only if the left branch returns `NotMatched` do we even try the right branch. There's one interesting tidbit, though: the type function `Server (left :<|> right)` is a structure of functions, `Server left :<|> Server right`. This means that the handler we're using on either branch is of the correct type, always!

\begin{code}
instance (HasRoute left, HasRoute right) => HasRoute (left :<|> right) where
    route _ (leftHandler :<|> rightHandler) = \request -> do

        let tryLeft  = route (Proxy :: Proxy left) leftHandler
            tryRight = route (Proxy :: Proxy right) rightHandler

        leftMatch <- tryLeft request
        case leftMatch of
            Matched result -> pure $ Matched result
            NotMatched -> tryRight request
\end{code}

I'd like you to know that it was very hard to resist refactoring this to use [`Alternative`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Applicative.html#t:Alternative), but this post is already getting long.

Now we have the appropriate blocks to serve our API. In case none of the routes match, we need to build a HTTP404 response:

\begin{code}
notFound :: Response
notFound = responseLBS HTTP.status404 mempty mempty
\end{code}

and beyond this, we just need to provide the `Server api` structure to a new function, `serve`:

\begin{code}
serve :: (HasServer api, HasRoute api)
      => Proxy api
      -> Server api
      -> (Request -> IO Response)
serve proxy server = \request -> do
    result <- route proxy server request
    case result of
        Matched response -> pure response
        NotMatched -> pure notFound
\end{code}

So, what does `Server api` look like? Well, it's a structure of handler functions, one for each endpoint. We'll write them all out with trivial logic:

\begin{code}
-- To serve GET /forecast/lastupdated
forecastLastUpdated :: IO UTCTime
forecastLastUpdated = Data.Time.getCurrentTime

-- To serve GET /forecast/<date>/temperature
forecastTemperatureAtDate :: Day -> IO Temperature
forecastTemperatureAtDate _ = pure $ MkTemperature 0

-- To serve GET /weather/temperature&city=<city>
temperatureAtCity :: City -> IO Temperature
temperatureAtCity _ = pure $ MkTemperature 0

-- To serve POST /weather/temperature/<city>
recordTemperatureAtCity :: City -> Temperature -> IO ()
recordTemperatureAtCity _ _ = pure () -- This is technically ACID-compliant
\end{code}

We need to combine these four handlers in the same way that the endpoints are structured:

```haskell
apiServer :: Server API
apiServer =
    let forecastAPI = forecastLastUpdated :<|> forecastTemperatureAtDate
        weatherAPI = temperatureAtCity :<|> recordTemperatureAtCity
    in forecastAPI :<|> weatherAPI
```

If you try this, you'll get a compilation error! This _actually happened_ as I was writing this blog post; I forgot that `QueryParam` in `temperatureAtCity` is actually optional! The type checker has my back, even when writing blog posts. Instead, I need to write:


\begin{code}
temperatureAtCityCorrect :: Maybe City -> IO Temperature
temperatureAtCityCorrect _ = pure $ MkTemperature 0

apiServer :: Server API
apiServer =
    let forecastAPI = forecastLastUpdated :<|> forecastTemperatureAtDate
        weatherAPI = temperatureAtCityCorrect :<|> recordTemperatureAtCity
    in forecastAPI :<|> weatherAPI
\end{code}

Now it compiles. Let's pull things all together:

\begin{code}
myServer :: Request -> IO Response
myServer = serve (Proxy :: Proxy API) apiServer
\end{code}

Now that we have our server logic, we defer the parsing of requests, networking, and other low-level concerns to a lower-level server runner (in this case, `warp`). Our executable becomes:

\begin{code}
-- We convert our handler function to a WAI application, which is slightly
-- different for better resource management (not applicable here)
toApplication :: (Request -> IO Response) -> Application
toApplication handler = \request respond -> handler request >>= respond

runServer :: IO ()
runServer = Warp.run 80 (toApplication myServer)
\end{code}

<hr>

And voilà! We built a real HTTP web application using our simplified, homegrown version of Servant. This web server respects the API specification not out of politeness, but out of coercion by the compiler. Amazing!

The code above is admittedly a highly simplified version of what a production-ready web framework should be. Some of the differences between what I've shown you here and the real Servant include:

* Dealing with more failure modes;
* Dealing with content-types. Real Servant API specifications encode the accepted content-types, and automatically parse requests and serializes responses based on the type-level description of content types;
* Sublinear routing. Our routing algorithm finds a route in linear time, but the real Servant is a lot more clever;
* Support for more sophisticated API components, such as streaming endpoints and authentication;
* Running handler functions in monads _other_ than `IO`. This is a big deal if you have to wrangle side effects related to a database, logging facilities, OpenTelemetry, etc.

The next post is the last planned post in this series. It'll deal with _automatic derivation_ of client functions to query a server. See you soon!

