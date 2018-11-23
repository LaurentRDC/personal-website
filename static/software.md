---
title: Software
---

Below you will find a list of free and open-source software projects I am working on. You can see more of them on [GitHub](https://github.com/LaurentRDC) <i class="fab fa-github"/>

## iris-ued

<a href="https://pypi.org/pypi/iris-ued" target="_blank">
    <img src="https://img.shields.io/pypi/v/iris-ued.svg">
</a> 
<a href="https://anaconda.org/conda-forge/iris-ued" target="_blank">
    <img src="https://img.shields.io/conda/vn/conda-forge/iris-ued.svg">
</a>

![Overview of the GUI component of iris. Two GUI instances show the two types of datasets. On the top left, Bragg peak dynamics for photoexcited single-crystal data is shown. On the bottom right, azimuthally-averaged polycrystalline diffraction data is presented. Integration regions can be interactively dragged, updating the time-series shown below in real-time.](./images/iris_screen.png)

Ultrafast electron diffractometer generates huge amounts of data in the forms of image stacks. While processing this data is generally straightforward, the sheer size of datasets is always a problem. Even once the data is combined into a single time-series of images, we are still looking at multiple gigabytes of data.

Iris allows us to interactively look at this data by slicing diffraction patterns (and powder patterns) through time. Making use of our baseline-removal routine based on the dual-tree complex wavelet transform, we can look at publication-quality data minutes after data collection is complete. While each new experiment ultimately requires different tools, our investigations always start with Iris.

The latest version of Iris includes a plug-in manager. You can write your own plug-in to interact with raw, unprocessed ultrafast electron diffraction data and explore it.

To install from PyPI:

    > python -m pip install iris-ued

For Anaconda users, `iris-ued` is also available on the `conda-forge` channel:

    > conda install -c conda-forge iris-ued

## scikit-ued

<a href="https://pypi.org/pypi/scikit-ued" target="_blank">
    <img src="https://img.shields.io/pypi/v/scikit-ued.svg">
</a> 
<a href="https://anaconda.org/conda-forge/scikit-ued" target="_blank">
    <img src="https://img.shields.io/conda/vn/conda-forge/scikit-ued.svg">
</a>

`scikit-ued` is a fully-tested Python package containing routines and algorithms related to (ultrafast) electron diffraction. The package aims to provide software to deal with simulation, structure manipulation, image-analysis, baseline-determination, and more.

To install from PyPI:

    > python -m pip install scikit-ued

For Anaconda users, `scikit-ued` is also available on the `conda-forge` channel:

    > conda install -c conda-forge scikit-ued

## npstreams

<a href="https://pypi.org/pypi/npstreams" target="_blank">
    <img src="https://img.shields.io/pypi/v/npstreams.svg">
</a> 
<a href="https://anaconda.org/conda-forge/npstreams" target="_blank">
    <img src="https://img.shields.io/conda/vn/conda-forge/npstreams.svg">
</a>

`npstreams` is an open-source Python package for streaming NumPy array operations. The goal is to provide tested routines that operate on streams of arrays instead of dense arrays.

Streaming reduction operations (sums, averages, etc.) can be implemented in constant memory, which in turns allows for easy parallelization. Some routines in `npstreams` are parallelized in this way. In our experience, this approach has resulted in huge speedups when working with images; the images are read one-by-one from disk and combined/processed in a streaming fashion, in parallel.

To install from PyPI:

    > python -m pip install npstreams

For Anaconda users, `npstreams` is also available on the `conda-forge` channel:

    > conda install -c conda-forge npstreams

## pandoc-pyplot

<a href="http://hackage.haskell.org/package/pandoc-pyplot" target="_blank">
    <img src="https://img.shields.io/hackage/v/pandoc-pyplot.svg">
</a>
<a href="http://stackage.org/nightly/package/pandoc-pyplot" target="_blank">
    <img src="http://stackage.org/package/pandoc-pyplot/badge/nightly">
</a>

`pandoc-pyplot` is a Pandoc filter in the spirit of sphinx's [`plot_directive`](https://matplotlib.org/devel/plot_directive.html). Write Python code to make a Matplotlib figure in your document, and `pandoc-pyplot` will generate the figure for you and embed it in the document. This way, your documents are figures are always up-to-date with each other.