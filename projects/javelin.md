---
name: javelin
description: A Haskell implementation of series, or one-dimensional labeled arrays
repository: https://github.com/LaurentRDC/javelin
---

<a href="http://hackage.haskell.org/package/pandoc-plot" target="_blank">
    <img src="https://img.shields.io/hackage/v/javelin.svg">
</a>

Series implemented in the `javelin` package are a hybrid between arrays and associative maps.

Like `containers`'s `Map`, `Series` support efficient:

* random access by key ( $\mathcal{O}(\log n)$ );
* slice by key ( $\mathcal{O}(\log n)$ );

Like `vectors`'s `Vector`, `Series` support efficient:

* random access by integer index ( $\mathcal{O}(1)$ );
* slice by integer index ( $\mathcal{O}(1)$ );
* numerical operations as fast as contiguous arrays;