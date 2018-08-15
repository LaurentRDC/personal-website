---
title: "Writing GUIs with Python and PyQt5 (Part 2) - Qt signals and slots"
date: 2018-08-14
---

Have you ever wondered how you can write graphical desktop applications using Python? This is the second part of a tutorial series during which we will build a graphical user interface (GUI).

For the sake of concreteness, we will build a graphical user interface to the [`conda` package manager](https://conda.io/docs/).

## In this series

1. [Part 1 - Starting a project](./condagui-part1-skeleton.html)
2. [Part 2 - Qt signals and slots](./condagui-part2-signals.html)

## Qt Basics : Signals and Slots

As you can imagine, an interactive program must keep track of a lot of things. Based on your actions, for example, the program must react and update what you see, or perform some action. Sometimes, a single action, like clicking a button, can trigger many events within the program. To connect events (or actions) to reactions, Qt uses __signals and slots__.

A __signal__ is emitted when something of note has occured. The signal might or might not carry data in it. The best example of this is a `QPushButton`.

A `QPushButton` is a Qt widget. It can be added to other widgets -- for example our `QMainWindow`. When clicked, `QPushButtons` emit a signal called `clicked`, which carries no data. The signal can be connected to a function which will be called whenever the signal is emitted. This function is called a __slot__. Take a look at the following example:

```python
def click():
    print('Button has been clicked!')

# This example will not run on its own, but you will get the idea
button = QtWidgets.QPushButton()
button.clicked.connect(click)
```

In the above example, whenever the button `button`{.python} is clicked, the signal `button.clicked`{.python} is emitted, and the slot `click`{.python} will be called.

What about signal that can carry data? Again, think of a button that can be _toggled_ instead of simply clicked. `QPushButton` instances can be declared checkable, like a checkbox. In the case of a checkable button being checked or unchecked, the `toggled` signal is emitted -- along with the __new__ state of the button (`True` for checked, `False` for unchecked).

Let's modify the above example to have a checkable button:

```python
# This slot expects a boolean `on`.
def toggle(on):
    new_state = 'on' if on else 'off
    print('Button has been switched to ', new_state, '!')

# This example will not run on its own, but you will get the idea
button = QtWidgets.QPushButton()
button.setCheckable(True)       
button.toggled.connect(toggle)
```

Now, the signal we are interested in, `button.toggled`{.python}, carries with it a `bool`{.python} value. Therefore, the slot for it must also expect a boolean.

Python programs have no notion of type-checking at compilation time, but Qt is a C++ library. To help us connect signals and slots with the same expected type(s), we can use the `pyqtSlot`{.python} decorator. Let's rewrite the last example, with this new mechanism:

```python
# You'll need this extra bit for slots
from PyQt5 import QtCore

# This slot expects a boolean `on`.
@QtCore.pyqtSlot(bool)
def toggle(on):
    new_state = 'on' if on else 'off
    print('Button has been switched to ', new_state, '!')

# This example will not run on its own, but you will get the idea
button = QtWidgets.QPushButton()
button.setCheckable(True)       
button.toggled.connect(toggle)
```

In the above example, the signal and slot have matching types. This gives two benefits : 

1. Lower memory usage -- not important to us
2. Some level of safeguard against type errors

Therefore, for the remaining of this tutorial series, I will keep using the dedicated `pyqtSlot` decorators, and I encourage you to also use them. Finally, signals can be connected to many slots. Let's look at an example of this:

```python
# This slot expects a boolean `on`.
@QtCore.pyqtSlot(bool)
def toggle(on):
    new_state = 'on' if on else 'off
    print('Button has been switched to ', new_state, '!')

# Let's have another widget, a text-area with a fixed string
label = QtWidgets.QLabel('This is a text label')

# Define our button as checkable, like before
button = QtWidgets.QPushButton()
button.setCheckable(True)

# We can connect the same signal to multiple slots. These will be triggered at the same time
button.toggled.connect(toggle)             # Print the status of the button based on the toggled state
button.togged.connect(label.setVisible)    # Make the label visible if the button is checked, and invisible otherwise
```

The above example shows two things. Firstly, you can connect the same signal (in this case, `button.toggled`{.python} is a signal of type `bool`) to multiple slots. Secondly, widgets (like a `QLabel`) have built-in slots. In this case, `label.setVisible`{.python} is a `pyqtSlot` that accepts boolean values. All possible slots for built-in widgets are documented; see [here for the public slots of `QLabel`](https://doc.qt.io/qt-5/qlabel.html#public-slots).

Using signals and slots provides more than type-related benefits. The main draw for them is that signals and slots can be used to communicate between objects in _separate threads_. This will have great benefits for the responsiveness of our application: the thread waiting for user inputs -- the thread in which our `QMainWindow`{.python} lives -- will not be blocked when the application logic is busy. 

The examples above are fairly useless. Printing the status of buttons is only a demonstration. To use signals and slots for some real work, we will need to make use of a special type of objects, `QObject`{.python}s. It turns out that the machinery underlying signals and slots is built in an abstract base class called `QObject`{.python}. Any object that wants to send signals must ultimately inherit from this super class. For example, the `QPushButton`{.python} class we used above is a `QObject`. Our main window, of class `QMainWindow`, is also a QObject. All Qt widgets are `QObject`s, many non-widget things are also `QObject`s.

<hr>
 
That's all for today. We have a barebones package with an empty GUI, as well as theoretical knowledge of Qt signals and slots. At this point, don't panic if you haven't fully grasped the concept. Signals and slots _will come back often_.

Next time, we'll use signals and slots communicate between the graphical part of our program, and the logic controlling `conda`.