---
title: Can you make heterogeneous lists in Haskell? Sure — as long your intent is clear
date: 2021-09-26
updated: 2021-09-30
summary: Haskell's type system might seem restrictive sometimes. For example, heterogeneous lists are not allowed. In this post, I show how to build heterogenous collections using existential quantification -- which requires us to be explicit about our intent.
---

Sometimes, Haskell's type system seems a bit restrictive compared to dynamic languages like Python. The most obvious example is the heterogenous list:

```python
>>> # Python
>>> mylist = ["hello", "world", 117, None]
>>>
>>> for item in mylist:
...     print(item) 
hello
world
117
None
```

but in Haskell, list items must be of the same type:

```haskell
-- Haskell
mylist = ["hello", "world", 117, ()] -- Invalid: type cannot be inferred!
```

This is a contrived example, of course. But consider this use-case: I just want to print the content of the list. It's unfortunate I can't write:

```haskell
mylist :: Show a => [a]
mylist =  ["hello", "world", 117, ()] -- All these types have Show instances, but this won't compile
```

For this specific application, the type system is overly restrictive -- as long as all I want to do is print the content of my list! In this post, I'll show you how to do something like this using the `ExistentialQuantification` language extension.

## A more complex example

Let's say I want to list American football players. There are two broad classes of players (offensive and defensive) and we want to keep track of the players in a list -- the player registry. Our final objective is to print the list of players to standard output. 

Let's try to do the same in Haskell. Our first reflex might be to use a sum type:

```haskell
data Player = OffensivePlayer String String -- name and position
            | DefensivePlayer String String -- name and position

playerRegistry :: [Player]
playerRegistry = ...
```

However, not all sports stats apply to `OffensivePlayer` and `DefensivePlayer` constructors. For example:

```haskell
passingAccuracy :: Player -> IO Double
passingAccuracy (OffensivePlayer name pos) = lookupFromDatabase "passingAccuracy" name
passingAccuracy (DefensivePlayer name pos) = return 0 -- Defensive players don't pass


tacklesPerGame :: Player -> IO Double
tacklesPerGame (OffensivePlayer name pos) = return 0 -- Offensive players don't tackle
tacklesPerGame (DefensivePlayer name pos) = lookupFromDatabase "tacklesPerGame" name
```

The `Player` type is too general; we're not using the type system to its full potential. It's much more representative of our situation to use two separate types:

```haskell
data OffensivePlayer = OffensivePlayer String String
data DefensivePlayer = DefensivePlayer String String

passingAccuracy :: OffensivePlayer -> IO Double
passingAccuracy = ...

tacklesPerGame :: DefensivePlayer -> IO Double
tacklesPerGame = ...
```

This is much safer and appropriate. Now let's give ourselves the ability to print players:

```haskell
instance Show OffensivePlayer where
    show (OffensivePlayer name pos) = mconcat ["< ", name, " : ", pos, " >"]

instance Show DefensivePlayer where
    show (DefensivePlayer name pos) = mconcat ["< ", name, " : ", pos, " >"]
```

Awesome. One last problem:

```haskell
-- This won't typecheck
playerRegistry = [ OffensivePlayer "Tom Brady"       "Quarterback"
                  , DefensivePlayer "Michael Strahan" "Defensive end"
                  ]

printPlayerList :: IO ()
printPlayerList = forM_ playerRegistry print -- `forM_` from Control.Monad
```

Rather annoying. We could wrap the two player types in a sum type:

```haskell
data Player = OP OffensivePlayer
            | DP DefensivePlayer

instance Show Player where
    show (OP p) = show p
    show (DP p) = show p

playerRegistry :: [Player]
playerRegistry = [ OP (OffensivePlayer "Tom Brady"       "Quarterback")
                 , DP (DefensivePlayer "Michael Strahan" "Defensive end")
                 ]

printPlayerList :: IO ()
printPlayerList = forM_ playerRegistry print        
```

but this is quite clunky. It also doesn't scale well to cases where we have a lot more types! 

## Enter existential quantification

The latest version of the Haskell language (Haskell 2010) is somewhat dated at this point. However, the Glasgow Haskell Compiler supports [language extensions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html) at the cost of portability. Turns out that the [`ExistentialQuantification`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html#existentially-quantified-data-constructors) language extension can help us with this problem. 

We turn on the extension at the top of our module:

```haskell
{-# LANGUAGE ExistentialQuantification #-}
```

and create an existential datatype:

```haskell
data ShowPlayer = forall a. Show a
                => ShowPlayer a
```

The datatype `ShowPlayer` is a real datatype that bundles any data `a` which can be shown. Note that **everything else** about the internal type is forgotten, since the `ShowPlayer` type wraps **any** type that can be shown (that's what `forall a. Show a` means).

We can facilitate the construction of a `Player` with the following helper function:

```haskell
mkPlayer :: Show a => a -> ShowPlayer
mkPlayer a = ShowPlayer a show 
```

Now since the data bundled in a `ShowPlayer` can be shown, the only operation supported by `ShowPlayer` is `Show`:

```haskell
instance Show ShowPlayer where
    show (ShowPlayer a) = show a
```

Finally, our heterogenous list:

```haskell
playerRegistry :: [ShowPlayer]
playerRegistry = [ -- ✓ OffensivePlayer has a Show instance ✓
                   ShowPlayer (OffensivePlayer "Tom Brady"       "Quarterback"))
                   -- ✓ DefensivePlayer has a Show instance ✓
                 , ShowPlayer (DefensivePlayer "Michael Strahan" "Defensive end"))
                 ]

printPlayerList :: IO ()
printPlayerList = forM_ playerRegistry print 
```

So we *can* have an heterogenous list -- as long as the only thing we can do with it is show it!

The advantage here compared to the sum-type approach is when we extend our code to many more types:

```haskell
data Quarterback    = Quarterback  String deriving Show
data Lineman        = Lineman      String deriving Show
data Runningback    = Runningback  String deriving Show
data WideReceiver   = WideReceiver String deriving Show

data DefensiveEnd   = DefensiveEnd String deriving Show
data Linebacker     = Linebacker   String deriving Show
data Safety         = Safety       String deriving Show
data Corner         = Corner       String deriving Show


-- Example: some functions are specific to certain positions
passingAccuracy :: Quarterback -> IO Double
assingAccuracy = ...


playerRegistry :: [ShowPlayer]
playerRegistry = [ mkPlayer (Quarterback  "Tom Brady"))
                 , mkPlayer (DefensiveEnd "Michael Strahan"))
                 , mkPlayer (Safety       "Richard Sherman"))
                 , ...
                 ]

printPlayerList :: IO ()
printPlayerList = forM_ playerRegistry print 
```

This way, we can keep the benefits of the type system when we want it, but also give ourselves some flexibility when we need it. This is actually similar to object-oriented programming, where classes bundle data and operations on them into an **object**!

## A bit more functionality

Let's pack in more operations on our heterogenous list. We might want to not only show players, but also access their salaries. We describe the functionality common to all players in a typeclass called `BasePlayer`:

```haskell
class Show p => BasePlayer p where
    -- Operate in IO because of database access, for example
    getYearlySalary :: p -> IO Double

instance BasePlayer Quarterback where
    ...

instance BasePlayer Lineman where
    ...
```

We can update our player registry to support the same operations as `BasePlayer` through the `Player` existential type:

```haskell
data Player = forall a. BasePlayer a
            => Player a

instance Show Player where
    show (Player a) = show a

instance BasePlayer Player where
    getYearlySalary (Player a) = getYearlySalary a
```

and our new heterogenous list now supports:

```haskell
playerRegistry :: [Player]
playerRegistry = [ Player (Quarterback  "Tom Brady")
                 , Player (DefensiveEnd "Michael Strahan")
                 , Player (Safety       "Richard Sherman")
                 , ...
                 ]

printPlayerList :: IO ()
printPlayerList = forM_ playerRegistry print -- unchanged


average_salary :: IO Double
average_salary = do
    salaries <- for playerRegistry getYearlySalary -- (`for` from Data.Traversable)
    return $ (sum salaries) / (length salaries)
```

So we can have a heterogenous list -- but we can only perform operations which are supported by the `Player` type. In this sense, the `Player` type encodes our *intent*.

## Conclusion

In this post, we've seen how to create heterogenous lists in Haskell. However, contrary to dynamic languages, we can only do so *provided we are explicit* about our intent. That means we get the safety of strong, static types with some added flexibility if we so choose.

If you're interested in type-level programming, including but not limited to the content of this present post, I strongly recommend Rebecca Skinner's [An Introduction to Type Level Programming](https://rebeccaskinner.net/posts/2021-08-25-introduction-to-type-level-programming.html) 

*Thanks to [Brandon Chinn](https://brandonchinn178.github.io/blog/) for some explanation on how to simplify existential types*.