---
title: "Servant by construction - a blog series"
date: 2025-09-26
summary: I introduce a new blog series where we will learn about Servant, a Haskell web framework powered by cutting-edge type features, by building simple versions of its important features.
tags: haskell, web, software engineering, servant by construction
---

A large fraction of software engineers work in web development.

I also do some web development these days, exclusively on the backend. Backend software engineers, just like their frontend counterparts, have streamlined their work by packaging development utilities into *frameworks*. Even in the relatively small Haskell community, there are many web frameworks, ranging from quick-to-start ([scotty](https://github.com/scotty-web/scotty)) to fully-featured-and-opinionated ([Yesod](https://www.yesodweb.com/) and [IHP](https://ihp.digitallyinduced.com/)).

The Haskell community has historically been fertile ground for innovation; web development is no different. One framework in particular has pushed the boundary of what it means to write web applications: [Servant](https://www.servant.dev/).
Don't be fooled by the deceptively simplistic website: Servant is one of the crown jewels of the Haskell ecosystem. It combines the broad appeal of writing web applications, with the use of some of Haskell's most advanced type system features to perform incredible feats of engineering.

Servant is worth discussing and studying for two reasons.

The first is to use Servant as a concrete application of type-level computation. In this respect, I hope readers can take some of the design ideas from Servant and bring it back to their community, just like [QuickCheck](https://hackage.haskell.org/package/QuickCheck) pioneered property-based testing.
The second is that while Servant is powerful, it remains a rather sophisticated piece of software that is hard to learn and understand. Even with some Haskell experience, Servant is not yet a pick-up-and-go tool; wandering off the beaten path (i.e. doing anything not covered in the [documentation](https://docs.servant.dev/)) remains a daunting prospect for many, as it was for me. By studying the inner workings of Servant, we can learn about how to use it, and extend it, effectively.

I want to celebrate the boldness of Servant, while teaching you about its ways. This is why I am starting a *blog series*, where I will cover the basics of Servant by *implementing* a simple version of Servant's internals, as literate Haskell files. By *constructing* Servant, we will both learn various techniques involving Haskell's advanced type system, but also demystify the real Servant library.

This blog series will build in complexity. We will start by representing an API as a type. This type won't be useful straight away, but subsequent blog posts will showcase one capability of Servant by using this type, starting with generating type-safe links to endpoints, and culminating somewhere I haven't decided yet.

Let's go!

#### Blog posts in this series


* [An API as a type](/posts/servant-by-construction/ApiAsType.html)
* [Type-safe links](/posts/servant-by-construction/TypeSafeLinks.html)
