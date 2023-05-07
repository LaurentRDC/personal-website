---
title: The algebraic structure of a trading stop-loss system
date: 2023-05-07
summary: I'm building a trading stop-loss system, and I have discovered that it forms a neat algebraic structure.
---

I was once an undergraduate student in a joint Mathematics & Physics program. Some of the math courses, namely group theory and algebra, remained very abstract to me throughout my education. There is some group theory in the description of symmetries of physical systems; but being an experimentalist, I didn't use more than 5% of what I learned in my undergrad during my PhD.

However, in the course of my work now in finance, I had the pleasure of discovering that I was actually working with an algebraic structure. This post describes how that happened.

------------------------------------------

The small trading firm for which I work is focusing a bit more on automated performance monitoring these days. With detailed trading performance data streaming in, it is now a good time to implement a *stop-loss system*.

A stop-loss system is a system which receives trading performance data, and emits three categories of signal:

* an all-clear signal, meaning that nothing in recent trading performance indicates a problem;
* a warning signal, meaning that recent trading performance is degraded -- but not yet concerning -- and a human should take a look under the hood;
* a halt signal, meaning that there is most probably something wrong, trading should be halted at once.

Of course, we're trading different products in different markets and even jurisdictions, and therefore the trading performance of every product is monitored independently. Moreover, our risk tolerance or expectations may be different for every product, and so a stop-loss system is really a framework in which to express multiple stop-loss rules, with different products being supervised by completely different stop-loss rules.

Let us consider examples: assume that we're trading a particular stock like AAPL[^stock]. Sensible stop-loss rules might be:

* If our current position has lost >10% in value over the last month, emit a warning; if the position has lost >25% over the last month, emit a halt signal.
* If we're expecting market volatility in the next hour to be high (for example, due to expected high-impact news), emit a halt signal.
* If our forecast of the ticker price is **way off** -- perhaps due to a problem in the forecasting model --, emit a halt signal.

Here is what a rule framework might looks like[^python]:

```python
from enum import Enum, auto, unique
from typing import Callable

@unique
class Signal(Enum):
    AllClear = auto()
    Warn     = auto()
    Halt     = auto()

class Context:
    ...

Rule = Callable[[Context], Signal]

# Example rule
def rule(context: Context) -> Signal:
    ...
```

A `Rule` is a function from some `Context` object to a `Signal`. We're packing all information required to make decisions in a single data structure for reasons which will become obvious shortly. In this framework, we may express one of the stop loss rule examples as:

```python
def rule(context: Context) -> Signal:
    recent_loss = loss_percent( context.recent_performance(period="30d") )
    if recent_loss > 0.25:
        return Signal.Halt
    elif recent_loss > 0.10
        return Signal.Warn
    else:
        return Signal.AllClear
```

For the remainder of this post, I don't care anymore about the domain-specific content of a rule.

My colleagues and I are expecting that, in practice, we will have pretty complex rules. In order to build complex rules from smaller, simpler rules, I wanted to be able to compose `Rule`s together. This is straightforward because all rules have the same input and output types. Consider two rules, `rule1` and `rule2`. If I want a new rule to halt if both `rule1` and `rule2` emit `Signal.Halt`, I could write it like this:

```python
def rule1(context: Context) -> Signal:
    ...

def rule2(context: Context) -> Signal:
    ...

def rule_lax(context: Context) -> Signal:
    sig1 = rule1(context)
    sig2 = rule2(context)

    if sig1 == sig2 == Signal.Halt:
        return Signal.Halt
    elif sig1 == sig2 == Signal.Warn:
        return Signal.Warn
    else:
        return Signal.AllClear
```

That is an acceptable definition of rule composition. Since `rule_lax` will emit a `Halt` signal if both sub-rules emit a `Halt` signal, we'll call this type of composition *conjunction*. In order to make it more ergonomic to write, let us wrap all rules in an object and re-use the `&` (overloaded and) operator:

```python
from dataclasses import dataclass
from enum import Enum
from operator import attrgetter

class Signal(Enum):
    """
    Signals can be composed using (&):

    >>> Signal.AllClear & Signal.AllClear
    < Signal.AllClear: 1 > 
    >>> Signal.Warn & Signal.Halt
    < Signal.Warn: 2 > 
    >>> Signal.Halt & Signal.Halt
    < Signal.Halt: 3 >
    """
    AllClear = 1
    Warn     = 2
    Halt     = 3

    def __and__(self, other: "Signal") -> "Signal":
        return min(self, other, key=attrgetter('value'))

@dataclass
class rule(Callable):
    _inner: Callable[[Context], Signal]

    def __call__(self, context: Context) -> Signal:
        return self._inner.__call__(context=context)
    
    def __and__(self, other: "rule"):
        def newinner(context: Context) -> Signal:
            return rule1(context) & rule2(context)
        return self.__class__(newinner)
```

and now we can re-write `rule_lax` like so:

```python
# The @rule decorator is required in order to lift rule1 from a regular function
# to the `rule` object
@rule
def rule1(context: Context) -> Signal:
    ...

@rule
def rule2(context: Context) -> Signal:
    ...

rule_lax = rule1 & rule2
```

Now, `rule_lax` is defined such that it'll emit `Signal.Halt` if both `rule1` and `rule2` emit `Signal.Halt`. The same is true of warnings; if both rules emit a warning, then `rule_lax` will emit `Signal.Warning`. Here is a table which summarizes this composition:

| $A$ | $B$ | $A ~ \& ~ B$ |
|:---:|:---:|:------------:|
| $C$ | $C$ |      $C$     |
| $C$ | $W$ |      $C$     |
| $C$ | $H$ |      $C$     |
| $W$ | $C$ |      $C$     |
| $W$ | $W$ |      $W$     |
| $W$ | $H$ |      $W$     |
| $H$ | $C$ |      $C$     |
| $H$ | $W$ |      $W$     |
| $H$ | $H$ |      $H$     |

where $C$ is `Signal.AllClear`, $W$ is `Signal.Warning`, and $H$ is `Signal.Halt`. Therefore, `&` is a binary function from `Rule`s to `Rule`.

This is not the *only* natural way to compose rules. What about this?

```python
def rule_strict(context: Context) -> Signal:
    sig1 = rule1(context)
    sig2 = rule2(context)

    if (sig1 == Signal.Halt) or (sig2 == Signal.Halt):
        return Signal.Halt
    elif (sig1 == Signal.Warning) or (sig2 == Signal.Warning):
        return Signal.Warning
    else:
        return Signal.AllClear
```

In this case, `rule_strict` is more, uh, strict than `rule_lax`; it emits `Signal.Halt` if **either** `rule1` or `rule2` emits a stop signal. We'll call this composition *disjunction* and re-use the `|` (overloaded or) operator to make it more ergonomic to write:

```python
class Signal(Enum):
    """
    Signals can be composed using (&) and (|):

    >>> Signal.AllClear & Signal.AllClear
    < Signal.AllClear: 1 > 
    >>> Signal.Warn & Signal.Halt
    < Signal.Warn: 2 > 
    >>> Signal.Warn | Signal.Halt
    < Signal.Halt: 3 >
    """
    AllClear = 1
    Warn     = 2
    Halt     = 3

    def __and__(self, other: "Signal") -> "Signal":
        return min(self, other, key=attrgetter('value'))

    def __or__(self, other: "Signal") -> "Signal":
        return max(self, other, key=attrgetter('value'))

@dataclass
class rule(Callable):
    _inner: Callable[[Context], Signal]

    def __call__(self, context: Context) -> Signal:
        return self._inner.__call__(context=context)
    
    def __and__(self, other: "rule"):
        def newinner(context: Context) -> Signal:
            return rule1(context) & rule2(context)
        return self.__class__(newinner)

    def __or__(self, other: "rule"):
        def newinner(context: Context) -> Signal:
            return rule1(context) | rule2(context)
        return self.__class__(newinner)
```

With this implementation, we can express `rule_lax` and `rule_strict` as:

```python
# The @rule decorator is required in order to lift rule1 from a regular function
# to the `rule` object
@rule
def rule1(context: Context) -> Signal:
    ...

@rule
def rule2(context: Context) -> Signal:
    ...

rule_lax    = rule1 & rule2
rule_strict = rule1 | rule2
```

We can update the table for the definition of `&` and `|`:

| $A$ | $B$ | $A ~ \& ~ B$ | $A ~ | ~ B$ |
|:---:|:---:|:------------:|:-----------:|
| $C$ | $C$ |      $C$     |     $C$     |
| $C$ | $W$ |      $C$     |     $W$     |
| $C$ | $H$ |      $C$     |     $H$     |
| $W$ | $C$ |      $C$     |     $W$     |
| $W$ | $W$ |      $W$     |     $W$     |
| $W$ | $H$ |      $W$     |     $H$     |
| $H$ | $C$ |      $C$     |     $H$     |
| $H$ | $W$ |      $W$     |     $H$     |
| $H$ | $H$ |      $H$     |     $H$     |

So for a given a given `Context`, which is fixed when the trading stop-loss system is running, we have:

* A set of rule outcomes of type `Signal`;
* A binary operation called *conjunction* (the `&` operator);
    * `&` is associative;
    * `&` is commutative;
    * `&` has an identity, `Signal.Halt`;
    * `&` does NOT have an inverse element.
* A binary operation called *disjunction* (the `|` operator).
    * `|` is associative;
    * `|` is commutative;
    * `|` has an identity, `Signal.AllClear`;
    * `|` does NOT have an inverse element.

That looks like a commutative semiring to me! Just a few more things to check:

* `|` distributes from both sides over `&`: 
    * $a ~|~ (b ~\&~ c)=(a ~|~ b) ~\&~ (a ~\&~ c)$ for all $a$, $b$, and $c$;
    * $(a ~ \& ~ b) ~|~ c = (a ~|~ c) ~\&~ (b ~\&~ c)$ for all $a$, $b$, and $c$.
* The identity element of `&` (called $0$, in this case `Signal.Halt`) annihilates the `|` operation, i.e. $0 ~ | ~ a = 0$ for all $a$.

Don't take my word for it, we can check exhaustively:

```python
from itertools import product

zero = Signal.Halt
one  = Signal.AllClear

# Assert & is associative
assert all( (a & b) & c == a & (b & c) for (a, b, c) in product(Signal, repeat=3)  )
# Assert & is commutative
assert all( a & b == b & a for (a, b) in product(Signal, repeat=2)  )
# Assert & has an identity
assert all( a & zero == a for a in Signal )

# Assert | is associative
assert all( (a | b) | c == a | (b | c) for (a, b, c) in product(Signal, repeat=3)  )
# Assert | has an identity
assert all( a | one == a for a in Signal )

# Assert | distributes over & on both sides
assert all( a | (b & c) == (a | b) & (a | c) for (a, b, c) in product(Signal, repeat=3)  )
assert all( (a & b) | c == (a | c) & (b | c) for (a, b, c) in product(Signal, repeat=3)  )

# Assert identity of & annihilates with respect to |
assert all( (zero | a) == zero for a in Signal)
```

and there we have it! This design of a trading stop-loss system is an example of commutative semirings. This fact does absolutely nothing in the practical sense; I'm just happy to have spotted this structure more than 10 years after seeing it in undergrad.


[^stock]: I'm actually not involved in trading securities at all, but I think intuition about stock markets is more common

[^python]: I'll be using Python in this post because it was a requirement of the implementation, but know that I'm doing this under protest.
