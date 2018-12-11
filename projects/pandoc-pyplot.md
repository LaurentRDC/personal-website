---
name: pandoc-pyplot
repository: https://github.com/LaurentRDC/pandoc-pyplot
---

<a href="http://hackage.haskell.org/package/pandoc-pyplot" target="_blank">
    <img src="https://img.shields.io/hackage/v/pandoc-pyplot.svg">
</a>
<a href="http://stackage.org/nightly/package/pandoc-pyplot" target="_blank">
    <img src="http://stackage.org/package/pandoc-pyplot/badge/nightly">
</a>

`pandoc-pyplot` is a Pandoc filter in the spirit of sphinx's [`plot_directive`](https://matplotlib.org/devel/plot_directive.html). Write Python code to make a Matplotlib figure in your document, and `pandoc-pyplot` will generate the figure for you and embed it in the document. This way, your documents are figures are always up-to-date with each other.