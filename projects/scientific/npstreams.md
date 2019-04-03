---
name: npstreams - streaming operations on NumPy arrays
repository: https://github.com/LaurentRDC/npstreams
---

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