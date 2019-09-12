---
title: Use environments when writing Python code for a scientific publication
date: 2019-09-11
summary: For my most recent paper, I have had to write a lot of code. This code will need to be re-used in the near future, possibly by others. Here's how I ensure that the results can easily be reproduced.
---

My most recent paper, _Time- and momentum-resolved phonon spectroscopy with ultrafast electron diffuse scattering_ [^1], involves a lot of Python code that will no doubt be used by many students within our group in the future, and possible others in the community. This contrasts with my usual experience with research code, which is usually written as a one-off thing.

Imagine trying to reproduce the results from a paper. Maybe you are trying to reproduce your own results from a few years ago! You need three things: the original data, the scripts or programs, and the third-party software libraries. I don't need to convince you to save your data. However, I find that most graduate students depend implicitly on many software packages. There are big differences between versions of popular numerical Python libraries, e.g. `numpy` 1.6 vs `numpy` 1.12, or `matplotlib` 1.0 vs `matplotlib` 3.0. Did I also need `pywt`? I can't remember. 

Would you be able to re-create the computer _environment_ that was used to generate results? What about five years from now?

My approach to this problem involves Python environments. There are many tools to do this ([`conda`](https://conda.io/en/latest/), [`pyenv`](https://github.com/pyenv/pyenv) [`pipenv`](https://docs.pipenv.org/en/latest/), and most probably others). The one I use is `conda`, because it is so easy to use on all platforms (I use Windows).

Environments are created with a name (in this case, `myenv`), a python version, and packages you will need:

```
> conda create --name myenv python=3.7 numpy scipy matplotlib
```

Once your environment is created, you can enter it (or __activate__ it) using `activate`, and exit using `deactivate`:

```
> conda activate myenv
> (myenv) python --version
Python 3.7.3
> ( ... do your work ... )
> (myenv) conda deactivate
```

The workflow, when working on a particular science project, is then:

* Create an environment, if you haven't already
* Install packages you need
* Activate an environment
* Do your work
* Deactivate your environment

In order for someone (possibly you from the future on another computer) to recreate the same environment, you need to export it to file:

```
> conda env export --file myenv.yml
```

To re-create the environment elsewhere:

```
> conda env create --file myenv.yml
```

Keep one environment file per project, and you'll easily be able to reproduce results without any fuss. You future self will thank you. You can read more about managing `conda` environments [here](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html).

[^1]: __L.P. René de Cotret__, J.-H. Pöhls, M. J. Stern, M. R. Otto, M. Sutton, and B. J. Siwick, _Time- and momentum-resolved phonon spectroscopy with ultrafast electron diffuse scattering_ (2019) Submitted. [Arxiv link](https://arxiv.org/abs/1908.02795).