---
name: Iris
repository: https://github.com/LaurentRDC/iris-ued
---

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