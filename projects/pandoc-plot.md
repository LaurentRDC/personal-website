---
name: pandoc-plot
description: A Pandoc filter to generate figures directly in documents, using your plotting toolkit of choice
repository: https://github.com/LaurentRDC/pandoc-plot
---

<a href="http://hackage.haskell.org/package/pandoc-plot" target="_blank">
    <img src="https://img.shields.io/hackage/v/pandoc-plot.svg">
</a>
<a href="https://anaconda.org/conda-forge/pandoc-plot" target="_blank">
    <img src="https://img.shields.io/conda/vn/conda-forge/pandoc-plot.svg">
</a>

`pandoc-plot` is a Pandoc filter that allows to keep documents and figures always in sync. Write plotting code to make a figure in your Markdown , and `pandoc-plot` will generate the figure for you and embed it in the document. This way, your documents and figures are always up-to-date with each other.

`pandoc-plot` supports many plotting toolkits, including [matplotlib](https://matplotlib.org/), [MATLAB](https://www.mathworks.com/), [GNU Octave](https://www.gnu.org/software/octave/), [ggplot2](https://ggplot2.tidyverse.org/), [Julia's `Plots.jl`](https://docs.juliaplots.org/latest/), and many more! See the [website for examples and documentation](https://laurentrdc.github.io/pandoc-plot/).