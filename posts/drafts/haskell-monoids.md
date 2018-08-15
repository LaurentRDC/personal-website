---
title: "Interfaces in Haskell : Monoids"
date: 2018-08-10
---

To a beginner like me, the type system in Haskell can be intimidating. Writing Python seems so flexible compared to Haskell. If types have to match, are we not loosing wiggle room? Maybe in a statically-typed language like C. However, just like Python has interfaces, so does Haskell. In fact, interfaces in Haskell are even more powerful thanks to compiler guarantees!

In this post I want to talk about one such interface with a scary name: the Monoid typeclass.

## Monoids in Mathematics

Monoids come from pure mathematics. In fact, if you are familiar with group theory, you have already seen monoids[^group].

A __monoid__ is the collection of values together with an operation that allows to _combine_ two values together[^semigroup]. Moreover, there should be a value that we consider _empty_ or _null_.

Let's look at the example of numbers. I can think of two possible monoids we can make with integers {0, 1, -1, 2, -2, ...}:

* Integers, together with the operation of addition ($+$), forms a monoid because there is a null element (0): for any integer $a$, $a + 0 \equiv a$.
* Integers, together with the operation of multiplication ($\times$), forms a monoid because there is a null element (1): for any integer $b$, $b \times 1 \equiv b$

[^group]: A group is a monoid with the added property that every element has an inverse (with respect to the monoid operation) that is inside the monoid as well

[^semigroup]: If the definition stopped here, we would have the definition of a semigroup.

## Monoids in Haskell

Monoids in Haskell are the same as monoids in Mathematics. Take the example of `String`{.haskell}s:

* The collection of all possible `Strings`{.haskell}s contains an empty element, `""`{.haskell}. That's a good start.
* The 'natural' combination of two strings is string concatenation (`++`{.haskell}). Note that for any string `s`, `s ++ ""`{.haskell} is `s`

Therefore, the set of all possible strings, together with the string concatenation operation and the empty string `""`{.haskell}, forms a monoid!

In fact, almost all collections (lists, sets, `String`{.haskell}, `Text`{.haskell}) are monoids under concatenation, as long as there is an empty element. In contrast, if there existed a [type for non-empty lists](https://hackage.haskell.org/package/semigroups-0.16/docs/Data-List-NonEmpty.html), this type could not form a monoid under concatenation because there is no _empty_ or _null_ element.

The monoid operation is called `mappend`{.haskell} or `(<>)`{.haskell}, and the empty monoid element is called `mempty`{.haskell}. Using `String`{.haskell}s again:

```haskell
import Data.Monoid ((<>))                               -- mappend and mempty are in prelude, no need to import them

alternatives = [ "Monoids are great!" 
               , mappend "Monoids " "are great!"        
               , mappend "Monoids are great!" mempty    -- mempty in this case is the empty string ""
               , "Monoids " `mappend` "are great!"      -- Infix usage of mappend
               , "Monoids " <> "are great!" ]           -- Pure elegance

-- Check that all elements in `alternatives` are equal to the first one 
allEquivalent = and $ map ( == first) (tail alternatives) -- True
    where first = head alternatives
```