---
title: "Writing GUIs with Python and PyQt5 (Part 1) - Starting a project"
date: 2018-08-14
---

Have you ever wondered how you can write graphical desktop applications using Python? This is the first part of a tutorial series during which we will build a graphical user interface (GUI).

For the sake of concreteness, we will build a graphical user interface to the [`conda` package manager](https://conda.io/docs/).

## In this series

1. [Part 1 - Starting a project](./condagui-part1-skeleton.html)
2. [Part 2 - Qt signals and slots](./condagui-part2-signals.html)

## Preliminaries : Conda

`conda` is a package manager best known for its built-in support for isolated environments. You can create an isolated environment with separate python versions easily:

```powershell
> # Create an environment named 'py3test' with the python==3.5 package
> conda create --name py3test python=3.5
>
> # Create an environment named 'py2test' with the python==2.7 package, as well as numpy and scipy
> conda create --name py2text python=2.7 numpy scipy
```

Within environments, you can manage dependencies -- without affecting other environments

```powershell
> # activate the environment (use 'source activate' on macOS and Linux)
> activate py3test
(py3test) >
(py3test) > # Install packages inside this environment only
(py3test) > conda install pyqt>5
```

For more information on conda features, take a look at the [user guide](https://conda.io/docs/user-guide/index.html). I choose to write a GUI for conda because some of my students might not be comfortable working in the command line.

## Creating a new package `condagui`

The first part of this tutorial is concerned with setting up the package. The name I have chosen is `condagui`. Therefore, our package will have the following file structure:

```
condagui/
+-- README.md
+-- LICENSE.txt
+-- setup.py
+-- condagui/
    +-- __init__.py
    +-- __main__.py
```

While the `condagui` package is empty, we have added a `__main__.py` script already. This is the file that will run when executing the package as `python -m condagui`, as described by [PEP 338 - Excuting modules as scripts](https://www.python.org/dev/peps/pep-0338/).

It is good practice to keep version and license information in `__init__.py`:

```python
# -*- coding: utf-8 -*-
__author__  = 'Laurent P. René de Cotret'
__email__   = 'laurent.decotret@outlook.com'
__license__ = 'BSD-3'
__version__ = '0.1.0'
```

## The setup script

In the `setup.py` file, we describe the package. Here is a short version of this file:

```python
from glob import glob
from os.path import basename
from os.path import splitext

from setuptools import find_packages
from setuptools import setup

setup(
    name='condagui',
    version='0.1.0',
    license='BSD 3-Clause License',
    description='GUI interface to the conda package manager',
    long_description='TODO',
    author='Laurent P. René de Cotret',
    author_email='laurent.decotret@outlook.com',
    url='https://github.com/LaurentRDC/condagui',
    packages=find_packages('condagui'),
    py_modules=[splitext(basename(path))[0] for path in glob('condagui/*.py')],
    include_package_data=True,
    zip_safe=False,
    classifiers=[
        # complete classifier list: http://pypi.python.org/pypi?%3Aaction=list_classifiers
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: Unix',
        'Operating System :: POSIX',
        'Operating System :: Microsoft :: Windows',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.5',    # As we will see later, we need Python 3.5+
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: Implementation :: CPython',
        'Topic :: Utilities',
    ],
    install_requires=[
        PyQt5                                       # We will use PyQt5, but PySide is almost equivalent
    ]
)
```

The most important thing for you to note right now is that we required `PyQt5`. We will come back to `setup.py` when we have something working.

## Package structure

I like to keep my `__init__.py` and `__main__.py` files as simple as possible. Moreover, we want to separate the app logic from the graphical display. Therefore, we will keep the graphical code in `condagui\gui.py` and the logic in `condagui\controller.py`.

Our package structure has changed to this:

```
condagui/
+-- README.md
+-- LICENSE.txt
+-- setup.py
+-- condagui/
    +-- __init__.py
    +-- __main__.py
    +-- gui.py
    +-- controller.py
```

## First step: empty window

In order to get a working prototype (that is utterly useless, but working nonetheless), we will write the main function. This is mostly boilerplate at this point.

The main function `run()`, will be defined in `__main__.py`:

```python
# -*- coding: utf-8 -*-

import sys

from PyQt5 import QtWidgets

from .gui import CondaGUI

def run():
    """
    Handle the events affecting CondaGUI in an event loop, and return when the window exits. All arguments
    are passed to an instance of CondaGUI.
    """
    app = QtWidgets.QApplication(sys.argv)
    window = CondaGUI()
    return app.exec_()

if __name__ == "__main__":
    run()
```

But what is this `CondaGUI` class we instantiated? This is the heart of our project, and is defined in `gui.py`:

```python
from PyQt5 import QtWidgets

class CondaGUI(QtWidgets.QMainWindow):

    def __init__(self, *args, **kwargs):
        # The superclass QMainWindow must be initialized
        super().__init__(*args, **kwargs)

        # The window must also be shown, otherwise nothing happens
        self.show()
```

This is the minimal package that can run. You can try it right now; run `python -m condagui` to see an empty window.

## Qt Basics : widgets

The above boilerplate might be meaningless to you. Most of it is not important at this time; however, magic happens during the definition of the `CondaGUI` class.

Graphical 'things' in Qt are called __widgets__. Widgets can be anything graphical, like a checkbox, or a button, or a progress bar. Widgets can also contain other widgets; For example, a checkbox with a label is really two widgets (the interactive checkbox, and the text label next to it). A `QMainWindow` (and by extension, a `CondaGUI` instance) is also a widget, containing all other widgets.

Before we continue on, it's important to know that all Qt objects are well-documented. However, there is no python-specific documentation; you must read the C++ documentation, and translate in your head. This sounds much worse than it is in practice. Take a look at the documentation for [`QMainWindow`](http://doc.qt.io/qt-5/qmainwindow.html). Take some time to read over the __detailed description__ section available [here](http://doc.qt.io/qt-5/qmainwindow.html#qt-main-window-framework).

<hr>
 
That's all for today. We have a barebones package with an empty GUI. You could re-use this package skeleton for any GUI project.

Next time, I'll explain some features of Qt that we will use to make this GUI interactive!