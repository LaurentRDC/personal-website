---
title: "An API as a type - Servant by construction part 1"
date: 2025-09-27
summary: This is the first post in a series on Servant, a Haskell framework for writing backend web applications.
tags: haskell, web, software engineering, servant by construction
---

This module is a Literate Haskell file, so we have to get some ceremony out-of-the-way.

\begin{code}
-- We'll get back to DataKinds
{-# LANGUAGE DataKinds #-}
module ApiAsType where

-- we'll need these later on
import Data.Text (Text)
import Data.Time (UTCTime, Day)
import GHC.TypeLits (Symbol)
\end{code}

A web application programming interface (API) can be represent by a tree of endpoints[^tree]. The foundational idea behind Servant is that this tree of endpoints can be represented as one type. This API type is constructed by using higher-order types to compose endpoints together.

[^tree]:  Although endpoints are usually not *shown* as a tree (for example, in an OpenAPI specification, endpoints are listed), routing requests is obviously more efficient if routes are *stored* as a tree.

This foundational idea leads to most of Servant's power. First, moving data into types helps tremendously with ensuring correctness, as we will soon see. Second, we can _derive_ functionality on demand for our API type using typeclasses. This last sentence is wholly uninformative unless you already know Servant, so please bear with me here while we construct examples.

For the remainder of this blog series, we will consider an example, concrete API. This isn't a lesson in API design. For simplicity, we assume that all data to and from the server will be encoded in plain text (although we may extend this in later posts).

The example API is a weather API which deals with measurements and forecasts of outdoor temperature. Here are the endpoints:

```txt
-- Returns the time at which forecasts we last updated.
GET /forecast/lastupdated

-- Returns the forecasted temperature on a (future) date for all known places.
-- The date is captured from the endpoint, e.g. "/forecast/2025-06-01/temperature"
-- should return the temperature forecasted for June 1st, 2025
GET /forecast/<date>/temperature

-- Returns the temperature on this current date. The city is expected to
-- be passed as a required query parameter, e.g. "/weather/temperature&city=Montreal"
GET /weather/temperature&city=<city>

-- Record a temperature measurement posted by someone for a given city
POST /weather/temperature/<city>
```

This example API has a good mixture of methods (GET and POST), query parameters, and path parameters. Let's create some types to help us describe this API.

First, we want to extend routes. For example, `/forecast/lastupdated` and `/forecast/<date>/temperature` share the root `forecast`:

\begin{code}
data a :> b

infixr 4 :>
\end{code}

Haskell is known for using all sorts of non-standard character sequences _as functions_, like `(>>=)`. Well, you can do the same for types! Example usage for `(:>)`:

```haskell
data Forecast
data LastUpdated

type LastUpdatedForecast = Forecast :> LastUpdated
```

Wait a second -- aren't path segments strings, like `"forecast"` and `"lastupdated"`? Why isn't the definition `data String :> String`? This is the power of Servant, but also the source of its complexity: APIs are meant to represented by *types*, that the compiler can check at compile-time. Instead of every piece of our API being a different string of the same type, they are all a different _type_. For now, trust me that this is worth doing.

APIs defined in Servant use tons of bespoke types, like `Forecast` and `LastUpdated` above. To save on the boilerplate of defining a bunch of these types, people typically use the [`DataKinds`](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/exts/data_kinds.html#extension-DataKinds) extension, which is enabled in this module. With it, instead of:

\begin{code}
data Root
data Branch
data Leaf

type Tree1 = Root :> Branch :> Leaf
\end{code}

we can write:

\begin{code}
type Tree2 = "root" :> "branch" :> "leaf"
\end{code}

`"root"`, `"branch"`, and `"leaf"` are type-level strings, which have kind `Symbol`, and you will see that they have advantages that we'll see later on.

Allright, now we can compose path segments into long path segments using `(:>)`. What about describing the endpoint? For example, `GET /forecast/lastupdated` should return the time at which the forecast was last updated. We can define a type to specify this:

\begin{code}
data Get a
\end{code}

where `a` is the return type of this endpoint. At last, we can express the full type of _one_ of our endpoints:

\begin{code}
type GetForecastLastUpdated = "forecast" :> "lastupdated" :> Get UTCTime
\end{code}

You might be wondering why our `Get` doesn't have a constructor. Why not:

```haskell
data Get a = MkGet {unGet :: a}
```

The reason is that our API will not exist in this form at runtime. As you will see later on, we will derive things from our API, and these derivations will be used at runtime, but not the API itself.

Let's tackle the second endpoint, `GET /forecast/<date>/temperature`. This endpoint is interesting because we want to *capture* some date, to be used when processing the request. To do this, we'll create yet another type:

\begin{code}
data Capture (name :: Symbol) a
\end{code}

where `name` will represent some parameter name, which will be useful later on, and `a` is the type being captured. Recall that in this case, `name` is a `Symbol`, or type-level string, and not a real `String`. Our `GET /forecast/<date>/temperature` can thus be represented by:

\begin{code}
-- Type representing temperature in Celsius
newtype Temperature = MkTemperature Double

type GetForecastTemperature = "forecast" :> Capture "date" Day :> "temperature" :> Get Temperature
\end{code}

The two API endpoints we've defined so far, have the same root (`forecast`). It's often useful to define relate endpoints sharing the same root together -- for example, to define authentication which should apply to all related endpoints. We will create a type to represent the branching of endpoints:

\begin{code}
data a :<|> b

-- We want :<|> to have a lower precedence than :> so that we can
-- write API definitions without tons of parentheses
infixr 3 :<|>
\end{code}

With `:<|>`, we can write our forecast endpoints as :

\begin{code}
type ForecastRoutes =
    "forecast" :>
            ( "lastupdated"
                    :> Get UTCTime
            :<|>
                Capture "date" Day
                    :> "temperature"
                        :> Get Temperature
            )
\end{code}

which is formatted to look like a tree:

```
forecast
 ├─lastupdated
 │  └─GET UTCTime
 └─<date>
    └─temperature
       └─GET Temperature
```

The next endpoint, `GET /weather/temperature&city=<city>`, has a new wrinkle: an (optional) query parameter. This warrants a new type:

\begin{code}
data QueryParam (name :: Symbol) a
\end{code}

such that we can write:

\begin{code}
newtype City = MkCity Text
type GetTemperature = "weather" :> "temperature" :> QueryParam "city" City :> Get Temperature
\end{code}

For the last endpoint, `POST /weather/temperature/<city>`, we need two last constructs. In a POST request, the *request body* will contain some data for the server to process (in the case of our endpoint, this data will be a temperature measurement in plaintext). Moreover, in our example, the POST request will return no data. For our API description to be adequate, we must describe the request body and the lack of data returned with -- you guessed it -- new types:

\begin{code}
data Post

data ReqBody a
\end{code}

The endpoint description becomes:

\begin{code}
type PostTemperature = "weather" :> "temperature" :> Capture "city" City :> ReqBody Temperature :> Post
\end{code}

Let's structure all of our endpoints into a single type:

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

Enough new types; let's recap what we have created so far:

* We created a type `:>` to build endpoint paths from segments;
* We created a type `:<|>` to build branching paths;
* We created a type `Capture` to capture data in endpoint paths;
* We created a type `QueryParam` to specify the name and type of optional query parameters;
* We created a type `ReqBody` to specify the type of the body of a POST request;
* We created types `Get` and `Post` to represent the verb of a request;
* We created a type, `API`, built up from smaller types `ForecastAPI` and `WeatherAPI`, which can be used to describe all of our API.

It bears repeating: none of the types defined above have constructors, which means that the type of our API, `API`, cannot even be instantiated at runtime! All functionality that will have to do with our API will come from deriving things from its definition **at compile time**.

In the next post, we will start from the `API` type and its endpoints, and derive something from them. This derivation will introduce us to the machinery that power much of Servant's functionality.
