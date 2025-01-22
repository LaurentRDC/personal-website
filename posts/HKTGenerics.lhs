---
title: Modeling dataframes in Haskell using higher-kinded types
date: 2025-01-22
summary: This post is an exploration of the type-level shenanigans that can be used to model dataframes in Haskell. I also show how to combine higher-kinded types with Haskell's automatic derivation mechanism
---

*Note: this document is a literate Haskell module!*

I am a firm believer in the purely functional programming approach, embodied by the Haskell programming language. While the Haskell 
community is not huge, it is large enough that I can work on most domains.

Most domains, but not all; data science remains hard to work on in Haskell. Since data science grew out not from software engineering,
but from students and scientists, the best data science tools are found in other communities such as [R](https://www.r-project.org/) and 
[Python](https://www.python.org/). If we focus further on machine-learning and """AI""" (ಠ_ಠ), then the distribution of high-quality 
tools is even more concentrated in the Python community.

I have started exploring what it would look like to build a Haskell-centric data science workflow more than a year ago, with the 
implementation of a [Series data structure](https://hackage.haskell.org/package/javelin-0.1.4.1/docs/Data-Series-Tutorial.html). 
While this was perfect for my use-case at the time (I wrote about it [here](/posts/rolling-stats.html) 
and [here](/posts/typesafe-tradingstrats.html)), the typical data scientist is used to columnar the data structure known as the 
*dataframe*.

Recently, an effort to [design a dataframe interface]((https://discourse.haskell.org/t/design-dataframes-in-haskell/11108)) in Haskell 
has been spearheaded by Michael Chavinda, with a focus on exploratory data science. This effort trades type safety for easier 
interactivity, similar to [Python's pandas DataFrames](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html).

In this blog post, I want to explore a different design tradeoff: what if one were to instead focus on type-safe expressiveness, with
no regards to interactivity? What would such a dataframe interface look like?

The design below is based on some intermediate type-level shenanigans. I was inspired by the approach that 
the [Beam SQL project](https://github.com/haskell-beam/beam) took, based on higher-kinded types.

Let's get imports out of the way:

\begin{code}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module HKTGenerics where

-- from `base`
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import GHC.Generics

-- from `vector`
import Data.Vector (Vector)
import qualified Data.Vector
\end{code}

Let's consider an example: we want to represent a set of people of some sort. We would normally represent
one person using a record type like so:

\begin{code}
data SimpleUser
    = MkSimpleUser { simpleUserFirstName :: String
                   , simpleUserLastName  :: String
                   , simpleUserAge       :: Int
                   }
\end{code}

Now, a *dataframe* of such users should be equivalent to:

\begin{code}
data FrameUser
    = MkFrameUser { frameUserFirstName :: Vector String
                  , frameUserLastName  :: Vector String
                  , frameUserAge       :: Vector Int
                  }
\end{code}

where each record is a `Vector` (similar to arrays in other languages), representing a column. This is the main draw of dataframes:
data is stored in columns, rather than simply e.g. `Vector SimpleUser`. This is not always best, but I will assume that the user 
has knowledge of the tradeoffs.

The structures of `SimpleUser` and `FrameUser` are extremely similar. We can use higher-kinded types to unify them by introducing a 
type parameter `f` which represents the container for values:

\begin{code}
data HKTUser f
    = MkHKTUser { hktUserFirstName :: f String
                , hktUserLastName  :: f String
                , hktUserAge       :: f Int
                }
\end{code}

Here, `f` has type `f :: Type -> Type`, just like the `Vector` type constructor. `HKTUser` is called a higher-kinded type, because 
unlike a type like `SimpleUser`, which has kind `Type`, `HKTUser` isn't a type, but a *type constructor*. I won't go into more detail
than this on higher-kinded types; consider watching the [Haskell Unfolder episode on the subject](https://www.youtube.com/live/EXgsXy1BR-0?si=9CMdb_wHJvCNfNYr).

Using this method of representing users, we can represent `SimpleUser` as `HKTUser Identity`, and `FrameUser` as `HKTUser Vector`.
Here, [`Identity`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Functor-Identity.html) is the trivial container.

One more thing before we move on. While `Identity` is a trivial functor, it still adds some overhead; `HKTUser Identity` and `SimpleUser`
aren't *exactly* equivalent. We can optimize this overhead away using a type family:

\begin{code}
type family Column (f :: Type -> Type) x where
    Column Identity x = x -- Optimization

    Column Vector x = Vector x
\end{code}

Finally, we can unify the representations of `SimpleUser` and `FrameUser`:

```haskell
data User f
    = MkUser { userFirstName :: Column f String
             , userLastName  :: Column f String
             , userAge       :: Column f Int
             }
```

The `Column` type family can be thought of a *type-level function*: `Column f a` is a type that depends on `f`. To be clearer, let's
create type synonyms:

\begin{code}
type Row   (dt :: (Type -> Type) -> Type) = dt Identity
type Frame (dt :: (Type -> Type) -> Type) = dt Vector
\end{code}

Using these synonyms, `Row User` is equivalent to `SimpleUser`. while `Frame User` is equivalent to `FrameUser`. We can operate on the
columns of dataframes easily, like so:

\begin{code}
-- | Returns the longest first name out of a dataframe of users.
-- If the dataframe is empty, returns the empty string.
longestFirstName :: Frame User -> String
longestFirstName = Data.Vector.foldl' longest mempty . userFirstName
    where
        longest :: String -> String -> String
        longest x y = if length x >= length y then x else y
\end{code}

With `longestFirstName`, we can glimpse the performance advantage of using dataframes: the `userFirstName` field is really
an array, and so finding the longest first name is an operation on an array of string rather than an array of `User`.

How can we build a dataframe? We can turn rows of, well, `Row User` into a single `Frame User` like so:
<!-- 
    Using the markdown syntax below will prevent the Haskell compiler
    from looking at the code
-->
```haskell
buildUserFrame :: Vector (Row User) -> Frame User
buildUserFrame vs 
    = MkUser { userFirstName = Data.Vector.map userFirstName vs
             , userLastName  = Data.Vector.map userLastName vs
             , userAge       = Data.Vector.map userAge vs
             }
```

This is a little tedious; for every type of dataframe, we need to write our own dataframe construction function! 
Can we write a function like `Vector (Row t) -> Frame t`{.haskell}, which works for any `t`? Yes we can.

Enter generics
--------------

*I want to thank __Li-yao Xia__ (Lysxia on the [Haskell Discourse](https://discourse.haskell.org/)) for helping me 
figure out how to do what you're about to read!*

If you squint, every type we would want to turn into a dataframe has the same structure: a record type where every record
is either `Vector a` or `Identity a` (nesting dataframes is out of scope for today). We can provide functionality for any 
suitable record type like that using Haskell generics[^syb].

[^syb]: R. Lämmel and S. Peyton Jones, *Scrap your boilerplate: a practical approach to generic programming*. ACM SIGPLAN International Workshop on Types in Language Design and Implementation (2003). [Link](https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-a-practical-approach-to-generic-programming) 

In Haskell, the `Generic` typeclass has nothing to do with "generics" in other programming language. "Generic" programming in Haskell is done 
with ad-hoc polymorphism (i.e. typeclasses). Instead, the `Generic` typeclass in Haskell is used to transform any datatype `t` into 
a generic representation, called `Rep t`, which can be used define functions which work over a large class
of types. Specifically, in our case, we want to create a function `Vector (Row t) -> Frame t`{.haskell} which works
for **any higher-kinded record type** `t` like `User`.

There are many explanations of Haskell's `Generic`, such as [Mark Karpov's Generics explained](https://markkarpov.com/tutorial/generics.html) 
blog post. The wrinkle in our dataframe problem is that types such as `User` are higher-kinded, and therefore some more care
is required.

Let's define our problem. We want to create a typeclass `FromRows` with a method, `fromRows`:

```haskell
class FromRows t where
    fromRows :: Vector (Row t) -> Frame t
```

We also want to provide a default definition of `fromRows` such that downstream users don't have to manually write instances
of `FromRows`:

```haskell
    default fromRows :: (???) => Vector (Row t) -> Frame t
    fromRows = ???
```

How can we write the default implementation of `fromRows`?

The key concept to remember is that `Generic` only works with types of kind `Type`, i.e. `Row User` but not `User`. For every
higher-kinded type `t` (like `User`), we care about two concrete types: `Row t` and `Frame t`. Therefore, we need to index our 
typeclass on both concrete types at once.

Enough word salad:

\begin{code}
class GFromRows r -- intended to be `Row`-like
                f -- intended to be `Frame`-like
    where  
    gfromRows :: Vector (r a) -> (f a)
\end{code}

We need to provide instances relating to some ([but not all](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Generics.html#g:14)) 
generic constructs, including `M1` (which is always required), `K1`, and `(:*:)`.

We start with the generic metadata type, `M1`:

\begin{code}
instance GFromRows r f => GFromRows (M1 i c r) (M1 i c f) where
    gfromRows :: Vector (M1 i c r a) -> M1 i c f a
    gfromRows = M1 . gfromRows . Data.Vector.map unM1
\end{code}

Then move on to `:*:`:

\begin{code}
instance ( GFromRows r1 f1
         , GFromRows r2 f2
         )
         => GFromRows (r1 :*: r2) (f1 :*: f2) where
    gfromRows vs = let (xs, ys) = Data.Vector.unzip 
                                $ Data.Vector.map (\(x :*: y) -> (x, y)) vs
                    in gfromRows xs :*: gfromRows ys
\end{code}

Finally, onto the representation of fields of constructors, `K1`:

```haskell
instance GFromRows (K1 i r) (K1 i f) where
    gfromRows = K1 . Data.Vector.map unK1
```

Note that the above will not compile. For the instances of `M1` and `:*:`, we assumed that `r` and `f` already had an instance 
of `GFromRows`. In the instance for `K1`, the compiler does not know about the relationship between `r` and `f` yet. In 
some sense, the instance involving the representation `K1` is the foundation on which other instances are defined. 

We can refine our instance by enforcing that `f` be an array of `r` using `(f ~ Vector r)`: 

\begin{code}
instance (f ~ Vector r) => GFromRows (K1 i r) (K1 i f) where
    gfromRows :: Vector (K1 i r a) -> K1 i f a
    gfromRows = K1 . Data.Vector.map unK1
\end{code}

We can now fill in the default implementation of `fromRows`:

\begin{code}
class FromRows t where
    fromRows :: Vector (Row t) -> Frame t
    
    default fromRows :: ( Generic (Row t)
                        , Generic (Frame t)
                        , GFromRows (Rep (Row t)) (Rep (Frame t))
                        ) 
                     => Vector (Row t) 
                     -> Frame t
    fromRows = to                   -- Turn `Rep (Frame t)` back into `Frame t`
             . gfromRows            -- Vector (Rep (Row t)) -> Rep (Frame t)
             . Data.Vector.map from -- Turn every row into a `Reo (Row t)`
\end{code}

How can we use this? Behold:

\begin{code}
data User f
    = MkUser { userFirstName :: Column f String
             , userLastName  :: Column f String
             , userAge       :: Column f Int
             }
    deriving (Generic) -- This is new
\end{code}

What's new here is that we need to derive a `Generic` instance for `User`. Then, the instance of `FromRows User` requires 
no method implementation:

\begin{code}
instance FromRows User
\end{code}

Then, we can re-implement `buildUserFrame` trivially:

\begin{code}
buildUserFrame :: Vector (Row User) -> Frame User
buildUserFrame = fromRows
\end{code}

Further work: nesting dataframes
--------------------------------

The implementation above works, but we assumed that every record type had fields of the form `Column f a`. 
How about nesting dataframes types? Consider this:

```haskell
data Address f
    = MkAddress { addressCivicNumber :: Column f Int
                , addressStreetName  :: Column f String
                }
    deriving (Generic)

instance FromRows Address
        
data Store f
    = MkStore { storeName    :: Column f String
              , storeAddress :: Address f
              }
    deriving (Generic)

instance FromRows Store -- type error
```

Ideally, we would want this `Store` type above to be equivalent to:

\begin{code}
data Store f
    = MkStore { storeName          :: Column f String
              -- Address gets unpacked into "adjacent" columns
              , addressCivicNumber :: Column f Int
              , addressStreetName  :: Column f String
              }
    deriving (Generic)
\end{code}

I haven't figured out how to do this yet, but it would be desirable.
 