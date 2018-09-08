---
title: "Writing GUIs with Python and PyQt5 (Part 3) - Application logic"
date: 2018-09-06
---

Have you ever wondered how you can write graphical desktop applications using Python? This is the second part of a tutorial series during which we will build a graphical user interface (GUI).

For the sake of concreteness, we will build a graphical user interface to the [`conda` package manager](https://conda.io/docs/).

## In this series

1. [Part 1 - Starting a project](./condagui-part1-skeleton.html)
2. [Part 2 - Qt signals and slots](./condagui-part2-signals.html)
3. [Part 3 - Application logic](./condagui-part3-controller.html)

## Application logic : the Controller object

It is a generally good idea to separate the application logic from its graphical representation. The graphical part of our application is located in the `QMainWindow`. The application logic of our application will be implemented as methods of a `Controller` object. 

One thing we need from our `Controller` is the ability to send and receive signals from our `QMainWindow` (for reasons that will be clear soon). The most general type that allows this is the `QtCore.QObject` class. Let's create a `controller.py` file:

```python
# controller.py
from PyQt5 import QtCore

class CondaController(QtCore.QObject):
    pass
```

Inheriting from `QtCore.QObject` allows us to define signals emitted from `CondaController`.

Let's add one feature : tell the graphical interface all available conda environments. A function is already given to use (`environments()`). How should we implement it in the `CondaController`?

First, let's make it a slot. We could request that the `CondaController` updates the available environments when we click a button. The available environments will be emitted in a signal. Here's the concrete implementation:

```python
# controller.py
from PyQt5 import QtCore

# The import below is from a separate package that interfaces with
# the conda package manager. The details of it are not important
# for this post
# You can find it here:
# https://github.com/LaurentRDC/condagui/blob/master/condagui/conda.py
from .conda import environments

class CondaController(QtCore.QObject):

    # Signals that the object can emit are defined at the class level

    # This signal is emitted with a set of available environments
    available_environments = QtCore.pyqtSignal(set)
    
    @QtCore.pyqtSlot()
    def update_available_environments(self):
        """ Emit an updated set of available environments. """
        envs = environments()
        self.available_environments.emit(envs)
```

The idea is that from the graphical interface, we can request the `CondaController` to get the available environments. This might take a while, so the environments are not returned, but rather emitted in a separate signal.

We need to include the `CondaController` in our `QMainWindow`:

```python
# gui.py
from PyQt5 import QtWidgets, QtCore
from .controller import CondaController

class CondaGUI(QtWidgets.QMainWindow):

    # Emit this whenever we want to know all available environments
    request_updated_available_environments = QtCore.pyqtSignal()

    def __init__(self, *args, **kwargs):
        # The superclass QMainWindow must be initialized
        super().__init__(*args, **kwargs)

        self.controller = CondaController()

        # While we have no way to emit `request_updated_available_environments` yet,
        # we'll want that signal to trigger the controller to update available environments
        self.request_updated_available_environments.connect(
            self.controller.update_available_environments
        )

        # We can emit the

        # The window must also be shown, otherwise nothing happens
        self.show()
```