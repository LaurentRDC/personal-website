---
name: repa
location: haskell-repa/repa
description: Regular parallel arrays in Haskell
repository: https://github.com/haskell-repa/repa
---

<a href="http://hackage.haskell.org/package/repa" target="_blank">
    <img src="https://img.shields.io/hackage/v/repa.svg">
</a>

The `repa` package provides high-performance, regular, multi-dimensional, shape-polymorphic arrays. Functions written with the `repa` combinators are automatically evaluated in parallel if possible.

The foundation of `repa` is described in the following publication:

<i class="ai ai-open-access"></i> Ben Lippmeier, Manuel Chakravarty, Gabriele Keller, and Simon Peyton Jones, _Guiding parallel array fusion with indexed types_, Proceedings of the 2012 Haskell Symposium (2012) [DOI:10.1145/2364506.2364511](https://doi.org/10.1145/2364506.2364511)
