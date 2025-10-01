---
title: "Type-safe links - Servant by construction part 2"
date: 2025-09-29
summary: In this post, we start slowly replicating Servant functionality by deriving type-safe links to endpoints defined as types. It introduces us to the way Servant is extended in general.
tags: haskell, web, software engineering, servant by construction
---

Recall [the previous post in this series](/posts/servant-by-construction/ApiAsType.html): we created a bunch of types, such as `Get`, `Capture`, and `(:>)`, to help us represent an API. So far, we can't do squat with these types, but that changes today. By the end of this post, we'll be able to do _one_ thing: we'll determine the string representation of an endpoint type.

I know, this isn't mindblowing, but it will require the use of a basic technique that most Servant functionality uses: the `Proxy` type. This tool (and others!) will be used again and again as we re-build more powerful components of Servant.

<hr>

This file is a Literate Haskell module, so we have to get some ceremony out-of-the-way.

\begin{code}
{-# LANGUAGE DataKinds #-}
module TypeSafeLinks where

-- From the previous post in this series
import ApiAsType
    ( (:>),
      Capture,
      City,
      Get,
      Post,
      QueryParam,
      ReqBody,
      Temperature,
    )

-- we'll need these later on
import Data.List (intersperse)
import Data.Proxy (Proxy(..))
import Data.Time (Day)
import GHC.TypeLits (KnownSymbol, symbolVal)
\end{code}

Here's what we want to do: given the type associated with an endpoint, we want a string that represents a [universal resource identifier](https://en.wikipedia.org/w/index.php?title=Uniform_Resource_Identifier&oldid=1278623925) to this endpoint, in the form of a *link*[^servant-extra].

[^servant-extra]: The Servant implementation is more powerful; it gives a type error if the link would point to something that isn't part of the API we specify. This introduces too much complexity for this blog post, but we may revisit this later.

Simply put, for every *segment* of an endpoint, we want a piece of string that represents it. For example, the endpoint represented by the type

```haskell
type ForecastLastUpdated = "forecast" :> "lastupdated" :> Get UTCTime
```

should give us the link

```haskell
"/forecast/lastupdated"
```

, while the endpoint represented by the type

```haskell
type ForecastTemperature = "forecast" :> Capture "date" Day :> "temperature" :> Get Temperature
```

should give us the link

```haskell
"/forecast/<date>/temperature"
```

Recall that the segments `"forecast"`, `"lastupdated"`, and `"temperature"` are _type-level strings_, or `Symbol`s. The links, such as `"/forecast/lastupdated"`, _will be represented_ by term-level strings:

\begin{code}
type Link = [String]
\end{code}

<hr>

Before we break out the advanced type stuff, let's build this functionality as if we represented our API not with a type, but with data. We'll focus on only one of the endpoints, specifically the endpoint given by the type:

```haskell
type ForecastLastUpdated = "forecast" :> "lastupdated" :> Get UTCTime
```

Imagine if we instead represented that endpoint using the following construction:

\begin{code}
data APIComponent
    = Get
    | Segment String
    | Join APIComponent APIComponent

exampleEndpoint :: APIComponent
exampleEndpoint = Segment "forecast" `Join` Segment "lastupdated" `Join` Get
\end{code}

Extracting the link from such a value can be done recursively:

\begin{code}
endpointLink :: APIComponent -> Either String Link
endpointLink = fmap (intersperse "/") . go [root]
  where
    root :: String
    root = "/"

    go :: Link -> APIComponent -> Either String Link
    go accumulator Get = Right accumulator
    go accumulator (Join (Segment segment) component) =
        go (accumulator <> [segment]) component
    -- Any other structure is considered a malformed API
    go _ _ = Left "Endpoint structure doesn't make sense"
\end{code}

(Sidenote: please forgive me for appending to a linked list; I'm trying to keep things as simple as possible.)

The result would be:
```haskell
 ghci> endpointLink exampleEndpoint
 "/forecast/lastupdated"
```

The key concept in `endpointLink` is that we recursively walk over the parts of the endpoint until we hit a verb, in this case `Get`, while accumulating parts of the link. When we build this functionality for type-level API definitions, we'll do something very similar: we'll walk over all components of the API type with a function, accumulating parts of the link, until we hit a terminal verb.

Note that we had to handle the case of nonsensical API structures at runtime. In this case, the only valid API are those composed of zero or more `Segment`s ultimately ending in `Get`, all joined with `Join`. Any other structure results in an error, represented by `Left "..."`.

<hr>

Let's move back to the world of APIs defined as types.

The component of an API type, like `Capture` or a type-level symbol `"forecast"`, is associated with zero or more segments. This type-level "case-by-case" is the realm of **typeclasses**. We want to create a typeclass, and apply it recursively over our API type, building from the base link `"/"` incrementally. Let's _try_ to create this typeclass:

```haskell
class HasLink1 endpoint where
    toLink1 :: Link -- ^ base link, initially just "/"
            -> Link

-- Example
instance HasLink1 "forecast" where
    toLink1 :: Link -> Link
    toLink1 baselink = [baselink, "forecast"]
````

Unfortunately, this is not going to compile. The problem is that type inference won't work; given a `toLink1` in the middle of a Haskell file, how can the compiler know which `endpoint` type it refers to? It can't know. We need to inject type information in the signature of `toLink1`.

How about this?

```haskell
class HasLink2 endpoint where
    toLink2 :: Link -- ^ base link
            -> endpoint
            -> Link
```

This would work, if we could construct a *term* of type `endpoint`... which we can't, since many of our API component types, like `Get a`, don't have constructors. The API component types are entirely for type-level computation; we can't construct values for them by design.

So, how do we inject type information, without an associated value? **Put down that `undefined`**   ಠ_ಠ   we don't do that here. Being respectable, modern Haskellers, we'll reach for [`Proxy`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Proxy.html#t:Proxy). `Proxy` is a higher-kinded type that allows us to carry a *type witness*, without having to construct any values (since the `Proxy` constructor takes no parameter). It's defined like so:

```haskell
data Proxy a = Proxy
```

We can now reformulate our class as the final, correct version:

\begin{code}
class HasLink endpoint where
    toLink :: Proxy endpoint
           -> Link -- ^ Base link
           -> Link
\end{code}

where the use of `Proxy` will allow the compiler to infer type information.

Now, we need to write typeclass instances of the class `HasLink` for all of the building blocks that make up our API. The easiest instances of `HasLink` are for the terminal verbs, as they do not appear in the endpoint link:

\begin{code}
instance HasLink (Get a) where
    toLink :: Proxy (Get a) -> Link -> Link
    toLink _ baselink = baselink

instance HasLink Post where
    toLink :: Proxy Post -> Link -> Link
    toLink _ baselink = baselink
\end{code}

There's something else about the terminal verbs, `Get` and `Post`: we expect them at the end of our endpoint. There's no recursion inside `toLink` here, because by definition, the description of an endpoint as a type always **ends** in `Get` or `Post`. These are the base cases.

On the other hand, query parameters and POST request bodies don't appear in the endpoint link either, but cannot be placed at the end position of the endpoint definition; they are always contained in a `(:>)`:

\begin{code}
instance HasLink sub => HasLink (ReqBody b :> sub) where
    toLink :: Proxy (ReqBody b :> sub) -> Link -> Link
    toLink _ = toLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (QueryParam name a :> sub) where
    toLink :: Proxy (QueryParam name a :> sub) -> Link -> Link
    toLink _ = toLink (Proxy :: Proxy sub)
\end{code}

Here, we see something different than for `Get` and `Post`: the definition of `toLink` recurs as it moves along the endpoint type.

Let's pause here for a second, because there's something very cool that's easy to miss: we can control what a valid API looks like by NOT writing instances for `HasLink`. For example, it doesn't make sense to create a link to _something_ involving `:<|>`; for example, `left :<|> right` is for composing endpoints `left` and `right` into an API, but there's no link that makes sense for `left :<|> right` itself. Well, if we don't write an instance `HasLink (a :<|> b)`, we'll never be able to construct a link to a malformed endpoint, because `toLink` won't compile! We've transformed the runtime error (`Left "..."`) from our function `endpointLink` above, into a **compile-time** error. How awesome is that?!

Let's climb down from this apotheosis, as it were, and move to the `HasLink` instance for the path components of endpoints, for which we've been using `Symbol`. Obviously, the type `("some-string" :: Symbol)` should result in the link fragment `("some-string" :: String)` somehow. This is one of the advantages of using `Symbol`: we can use [`symbolVal`](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-TypeLits.html#v:symbolVal) to get the value representation of the type-level string, which itself uses the [`KnownSymbol` constraint](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-TypeLits.html#t:KnownSymbol):

\begin{code}
instance KnownSymbol endpoint => HasLink endpoint where
    toLink proxy baselink = baselink <> [symbolVal proxy]
\end{code}

This is a somewhat complicated way of saying that the following example holds:

```haskell
toLink (Proxy :: Proxy ("endpoint" :: Symbol)) ["/"] == ["/", "endpoint"]
```

where `Proxy "endpoint"` contains the type-level string `"endpoint"`, while the right-hand side `["/", "endpoint"]` is a regular value of type `[String]`.

With this out of the way, we can write instances for all other types of our API. `<some symbol> :> subapi` is an interesting one because we recursively use `toLink` forwards and backwards:

\begin{code}
instance (KnownSymbol super, HasLink sub) => HasLink (super :> sub) where
    toLink (Proxy :: Proxy (super :> sub)) baselink
        =  toLink (Proxy :: Proxy super) baselink -- backwards
        <> toLink (Proxy :: Proxy sub) []         -- forwards
\end{code}

For example:

```haskell
toLink (Proxy :: Proxy ("forecast" :> "lastupdated")) ["/"] == ["/", "forecast", "lastupdated"]
```

`Capture` is last, and we get to re-use `symbolVal`. This is because we want to represent the name of the capture parameter, sandwiched between `<` and `>`:

\begin{code}
instance (HasLink sub, KnownSymbol name) => HasLink (Capture name a :> sub) where
    toLink (Proxy :: Proxy (Capture name a :> sub)) baselink
        =  baselink
        <> ["<" <> symbolVal (Proxy :: Proxy name) <> ">"]
        <> toLink (Proxy :: Proxy sub) []
\end{code}

For example:

```haskell
toLink (Proxy :: Proxy ("forecast" :> Capture "date" Day :> "temperature")) ["/"]
    == ["/", "forecast", "<date>", "temperature"]
```

We're almost done. We'll just wrap `toLink` to make it easier to use. In particular, we want to represent the list of link segments as a single piece of `String`:

\begin{code}
safeLink :: HasLink endpoint => Proxy endpoint -> String
safeLink endpoint
    = mconcat
    $ intersperse "/"
    $ toLink endpoint ["/"]
\end{code}


and boom! We are ready to generate links to our type-leven endpoints. Behold:

<!--
This comment prevents the following from being displayed, but it still gets typechecked

\begin{code}
_ = safeLink (Proxy :: Proxy ("forecast" :> Capture "date" Day :> "temperature" :> Get Temperature))
_ = safeLink (Proxy :: Proxy ("weather" :> "temperature" :> Capture "city" City :> ReqBody Temperature :> Post))
\end{code}
-->

```haskell
 ghci> safeLink (Proxy :: Proxy ("forecast" :> Capture "date" Day :> "temperature" :> Get Temperature))
 "/forecast/<date>/temperature"
 ghci> safeLink (Proxy :: Proxy ("weather" :> "temperature" :> Capture "city" City :> ReqBody Temperature :> Post))
 "/weather/temperature/<city>"
```

<hr>

This concludes our first derivation from an API that can be represented by types. This allowed us to create links to endpoints in our API, while ensuring that if an endpoint exists, links to it will be valid. Otherwise, we get a compile-time error!

I wanted to show you this first, because we got acquainted with [`Proxy`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Proxy.html#t:Proxy), a type that is often seen to help with inference of complex types.

The constructs you have seen in this post are all part of the Servant codebase, although I have simplified things here. This includes [`safeLink`](https://hackage.haskell.org/package/servant-0.20.2/docs/Servant-API.html#v:safeLink), and the [`HasLink`](https://hackage.haskell.org/package/servant-0.20.2/docs/Servant-API.html#t:HasLink) typeclass. The real [`safeLink`](https://hackage.haskell.org/package/servant-0.20.2/docs/Servant-API.html#v:safeLink) function is notably more powerful for two reasons:

* You get a compile time error if the endpoint isn't part of the given API. This means you get even more certainty that the link is valid;
* You can feed `safeLink` the values that go in `Capture` and `QueryParam`, to get the true link to the endpoint, rather than having e.g. a `<date>` placeholder.

In the next post, we will go one step further: checking the shape of the server handlers at compile-time. This will require us to use `Proxy` again, but we'll also learn about how type families, or type functions, fit in all of this.