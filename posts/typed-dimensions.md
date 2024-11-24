---
title: Scientific computing with confidence using typed dimensions
date: 2024-11-20
summary: I defer to my computer to check the validity of my scientific computations. In this post, I will describe typed dimensions, and showcase the software package that makes it possible.
---

I have performed non-trivial scientific calculations, in [university and beyond](/about.html), for almost 15 years.

Until the end of undergraduate school, this was mostly done by hand. Optimizing the [trajectory of a heavy ball](https://en.wikipedia.org/wiki/Brachistochrone_curve) or solving the [heat equation](https://en.wikipedia.org/wiki/Heat_equation) are problems with analytical solutions, and thus can be solved with pen(cil) and paper.

However, there were instances when numerical tools had to be used, rather than analytical ones. This occurred, for example, when replacing models of the physical world with measurements from the physical world in experimental physics classes. By the end of my B.Sc., [the need to take measurements, transform them, and report results was made much simpler by using a computer](/files/ugrad_project.pdf).

Fast forward to graduate school, and the amount of measurements (and their complexity) require the use of some of the most powerful computers I have ever used, even to this day. When implementing numerical routines, I had to pore over the equations (translated to computer expression) countless times, to ensure that they were correctly applied. Most importantly, __all units needed to be carefully checked by hand__. Small mistakes went unnoticed and wasted valuable resources. 
Take a look at [one of the computations](https://github.com/LaurentRDC/dissertation/blob/7fb658306b6c7dd1b4c8b983ad5f6d8fb91bcdb1/figures/graphite/eph-coupling.py#L56) from my Ph. D. dissertation ([PDF](/files/dissertation.pdf), [Git repository](https://github.com/LaurentRDC/dissertation)):

```python
from scipy.constants import physical_constants

def population(temperature: float, frequency: float) -> float:
    """
    Determine average phonon population at `temperature`.

    Parameters
    ----------
    temperature : float
        Temperature in Kelvin
    frequency : float
        Phonon frequency in Hertz
    """
    h, *_ = physical_constants["Planck constant over 2 pi in eV s"]
    kb, *_ = physical_constants["Boltzmann constant in eV/K"]
    return 0.5 * coth(h * frequency / (2 * kb * temperature)) - 0.5
```

This isn't a complex equation, but it showcases the problem perfectly. `temperature`, `frequency`, `h`, and `kb` are all floating-point numbers, with their units attached _as documentation_. This is not robust.

These days, instead of meticulously going over the details of calculations ahead of time, I now defer to my computer to check the validity of my calculations _as much as possible_. That's why computers exist: to free people from repetitive and error-prone work. In this post, I will describe a mechanism, typed dimensions, by which we can eliminate entire classes of bugs, and how the [`dimensional`](https://github.com/bjornbm/dimensional/) Haskell package makes this easy.

## Dimensions, units, and quantities

In the international system of units (also called SI units), there are 7 basic _dimensions_ a physical value can have:

* time duration $T$;
* length $L$;
* mass $M$;
* electric current $I$;
* thermodynamic temperature $\Theta$;
* amount of substance $N$;
* luminous intensity $J$.

Using the symbols associated with these base dimensions, we can express any dimension using exponents. For example, velocity (length over time duration) has dimensions $L/T = L \cdot T^{-1}$, while force (mass-length over time squared) has dimensions $M \cdot L \cdot T^{-2}$. Dimensionless quantities have all exponents being 0.

Why are dimensions important? [There are mathematical rules associated with combining numbers with dimensions](https://en.wikipedia.org/w/index.php?title=Dimensional_analysis&oldid=1254137729#Mathematical_properties):

* Two quantities with dimensions $D_1$ and $D_2$ can only be added or subtracted if $D_1 \equiv D_2$. For example, it does not make sense to add a length to a mass[^intensive].

* Any two quantities with dimensions $D_1$ and $D_2$ can be multiplied or divided, in which case the resulting dimension follows the usual rules of arithmetic. For example:

[^intensive]: [As pointed out on Hacker News](https://news.ycombinator.com/item?id=42202834#42213927), this is necessary but not sufficient. For example, intensive dimensions (such as thermodynamic temperature) cannot be meaningfully added together.

$$
\begin{align}
\left( L \cdot T^{-1} \right) / \left( L \cdot \Theta\right) 
    &= \left( L \cdot T^{-1} \right) \cdot \left( L^{-1} \cdot \Theta^{-1}\right) \\
    &= L^{1 - 1} \cdot T^{-1} \cdot \Theta^{-1} \\
    &= T^{-1} \cdot \Theta^{-1}
\end{align}
$$

From these two facts, we can derive some surprising conclusions. For example, the argument to many functions (such as sine, cosine, exponential, etc.) must be dimensionless! Consider the Taylor series expansion of the sine function:

$$
    \sin{x} = x - \frac{x^3}{3!} + \frac{x^5}{5!} - ...
$$

The argument $x$ must have the same dimensions as $x^3$, _which is only possible if and only if $x$ is a dimensionless quantity_.

Units of measure are physical manifestations of dimensions. Dimensions are abstract, while units of measure are concrete. For example, the length dimension $L$ can be measured in units of meters. Each basic dimension has its own canonical unit of measure, the _base unit_:

* The second (s) for time duration $T$;
* The meter (m) for length $L$;
* The kilogram (kg) for mass $M$;
* The ampere (A) for electric current $I$;
* The kelvin (K) for thermodynamic temperature $\Theta$;
* The mole (mol) for amount of substance $N$;
* The candela (cd) for luminous intensity $J$.

Units for non-basic dimensions, so-called _derived units_, are combinations of base units using the usual multiplication ($\cdot$) and division ($/$) operators. For example, velocity has dimensions of $L \cdot T^{-1}$, and can be measured in units of meters per second ($\frac{m}{s}$). Some derived units have names; for example, the Newton (N) is a derived unit corresponding to $\frac{\text{kg} \cdot \text{m}}{\text{s}^2}$.

Based on their dimensions, units can only be combined in certain ways. You can only add/subtract units of the same dimensions, while the multiplication/division of units modifies the dimensions.

Units can be converted to any other units of the same dimensions by conversion  _base unit_. For example, converting from units of imperial feet (ft) to kilometers (km) involves conversion from imperial feet to the base unit of length, the meter, and then conversion from the meter to the kilometer.

Finally, a _quantity_ is a number attached to units. 3.15 meter/second is a quantity with magnitude 3.15, units of meters/second, and dimensions of length over time duration (or $L \cdot T^{-1}$). 

Scientists take measurements of quantities, and run scientific computations to get answers in the form of other quantities. We can ensure correctness of these computations by realizing that __dimensions are known ahead of time__. This means that for statically- and strongly-typed languages (e.g. Haskell, Rust), we should enforce correctness using the type system.

## Type-safe dimensions

People's first contact with computational unit systems may come from [`pint`](https://github.com/hgrecco/pint), a Python library to manipulate quantities. While `pint` is better than nothing, it does not guarantee correctness at runtime due to Python's dynamic nature.

Instead, I prefer the [`dimensional`](https://github.com/bjornbm/dimensional/) package, a Haskell library created by [Björn Buckwalter](https://github.com/bjornbm) that guarantees correctness by ensuring type-safe dimensions at compile-time. Other tools exist for other languages -- for example, [F# famously includes units of measure](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure) as a first-party feature.

We will work by example to compute the [Maxwell-Boltzmann distribution](https://en.wikipedia.org/w/index.php?title=Maxwell%E2%80%93Boltzmann_distribution&oldid=1255523817). This is the distribution of velocities of identical particles of mass $m$, given some thermodynamic temperature $T$:

$$
    f(v) = \left( \frac{m}{2 \pi k_B T}\right)^{\frac{3}{2}} \exp{ \left( -\frac{m v^2 }{2 k_B T}\right) }
$$

where $v$ is the _magnitude_ of particle velocity in a three-dimensional space, and [$k_B$ is the Boltzmann constant](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure).

Without typed dimensions, the computation of this distribution can be done as follows:

```haskell
-- Boltzmann constant in Joules/Kelvin
boltzmannConstant_JpK :: Double
boltzmannConstant_JpK = 1.380649e-23


untypedMaxwellBoltzmannDist :: Double -- ^ Particle mass in kilogram
                            -> Double -- ^ Thermodynamic temperature in Kelvin
                            -> Double -- ^ Particle velocity in meters/second
                            -> Double -- ^ Probability density (dimensionless)
untypedMaxwellBoltzmannDist mass_kg temp_K velocity_mps 
    = ( mass_kg / (2 * pi * boltzmannConstant_JpK * temp_K) ) ** (3/2) 
    * exp ( - (mass_kg * velocity_mps **2) / (2 * pi * boltzmannConstant_JpK * temp_K) )
```

It's not too hard to check units by hand -- but it is tedious and error-prone. 

Now let's do this again, using [`dimensional`](https://github.com/bjornbm/dimensional/). 




<div class="notification is-warning" style="margin-bottom:3cm;margin-top:3cm">
<p>
Dear reader, be forewarned: <b>the rest of this post is not for the faint-of-heart</b>. 
</p>

<p>
Yes, this is overkill for simple calculations. And yet, the technique you are about to see has saved me numerous times. There's quite a bit more ceremony; in exchange, you get a lot more certainty about correctness. Software design is about trade-off, and this level of safety is required for some real-world applications.
</p>
</div>




Setup: I am using the [GHC2024 language edition](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/exts/control.html?highlight=language%20edition#extension-GHC2024). We will also replace the (implicitly imported) `Prelude` module to use [`Numeric.Units.Dimensional.Prelude`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html), which replaces the usual arithmetic operators to use dimension-aware ones, as well as importing the [`Unit`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#t:Unit), [`Dimension`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#t:Dimension), and [`Quantity`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#t:Quantity) types.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Numeric.Units.Dimensional.Prelude
```

To get acquainted, let's convert the definition of the Boltzmann constant. The dimensions of the Boltzmann constant are called [`DHeatCapacity`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Quantities.html#t:DHeatCapacity) in `dimensional`. Therefore, we can write:

```haskell
boltzmannConstant :: Quantity DHeatCapacity Double
```

which means that `boltzmannConstant` is a `Quantity` of dimension `DHeatCapacity`, whose magnitude is represented by a `Double`. We use the [`(*~)`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:-42--126-) operator to combine a number (`1.380649e-23`) with a compound unit, `joule / kelvin`, to get:

```haskell
boltzmannConstant :: Quantity DHeatCapacity Double
boltzmannConstant = 1.380649e-23 *~ (joule / kelvin)
```

We are not yet ready to implement the Maxwell-Boltzmann distribution function. One particular wrinkle is that exponents (such as the $3/2$ exponent of the first term) change the dimensions (i.e. the type) of the input. Therefore, some type arithmetic is required. To this end, `dimensional` provides [`(^)`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:-94-) to raise to an integer power, and [`sqrt`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:sqrt) to take the square root, while appropriately changing the dimensions at compile time. The type signatures are non-trivial to say the least, as it involves compile-time manipulation of types. The operation of "raising to the three-halfs power" becomes:

```haskell
raiseToThreeHalfsPower :: Floating a 
                       => Quantity d a 
                       -> Quantity (NRoot d Pos2 ^ Pos3) a
raiseToThreeHalfsPower x = (sqrt x) ^ pos3
```

where [`pos3`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:pos3) is a type-level integer. The dimension `(NRoot d Pos2 ^ Pos3)` will resolve to the appropriate dimension at compile-time via [type families](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#t:NRoot) once `d` is known.

Finally, we can implement the (dimensionally-typed) Maxwell-Boltzmann function:

```haskell
maxwellBoltzmannDist :: Mass Double
                     -> ThermodynamicTemperature Double
                     -> Velocity Double
                     -> Dimensionless Double
maxwellBoltzmannDist mass temp velocity 
    = raiseToThreeHalfsPower ( mass / (_2 * pi * boltzmannConstant * temp) )
    * exp ( negate (mass * velocity ^ pos2) / (_2 * pi * boltzmannConstant * temp) )
```

where [`_2`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:_2) is the dimensionless integer `2`, and `(*)`, `(/)`, `exp`, `negate`, and `^` are all dimensionally-aware operators from `dimensional` (rather than the `Prelude` module).

If you compile the program above, you'll get an error message ಠ_ಠ:

```
Couldn't match type ‘Neg3’ with ‘Zero’
  Expected: Dimensionless Double
    Actual: Quantity 
                (NRoot 
                    (DMass 
                        / ((Dim Zero Zero Zero Zero Zero Zero Zero * DHeatCapacity) * DThermodynamicTemperature)) 
                    Pos2 ^ Pos3
                ) 
                Double
(...snip...)
```

This is because I'm a sloppy physicist -- sorry! The Maxwell-Boltzmann distribution __is not a dimensionless quantity__: it actually returns a velocity density with dimensions of $1 / (L^3 \cdot T^{-3})$, or $L^{-3} \cdot T^{3}$. 

Let's fix this. There is no type synonym for velocity density in `dimensional`, but we can create one ourselves.

```haskell
type DVelocityCube = DVelocity ^ Pos3

type DVelocityDensity = Recip DVelocityCube -- dimension
type VelocityDensity = Quantity DVelocityDensity -- quantity
```

Our final version of `maxwellBoltzmannDist` becomes:

```haskell
maxwellBoltzmannDist :: Mass Double
                     -> ThermodynamicTemperature Double
                     -> Velocity Double
                     -> VelocityDensity Double
maxwellBoltzmannDist mass temp velocity 
    = raiseToThreeHalfsPower ( mass / (_2 * pi * boltzmannConstant * temp) )
    * exp ( negate (mass * velocity ^ pos2) / (_2 * pi * boltzmannConstant * temp) )
```

<a name="interactive-example"></a>
We can compute the result of `maxwellBoltzmannDist` in any compatible unit. We will do this for a gas of diatomic nitrogen. In an interactive Haskell session:

```haskell
ghci> import Numeric.Units.Dimensional.NonSI ( unifiedAtomicMassUnit ) -- unifiedAtomicMassUnit = amu
ghci>
ghci> let n2_mass          = 2     *~ unifiedAtomicMassUnit
ghci> let room_temperature = 300.0 *~ kelvin
ghci> let velocity         = 400   *~ (meter / second)
ghci>
ghci> maxwellBoltzmannDist n2_mass room_temperature velocity 
4.466578950309018e-11 m^-3 s^3
```
We can easily request specific output units using the [`(/~)`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:-47--126-) operator. In this case, we convert result to (hour / nautical mile)^3^:

```haskell
ghci> import Numeric.Units.Dimensional.NonSI ( nauticalMile, degreeRankine, knot )
ghci>
ghci> let n2_mass          = 2.6605E-27 *~ kilo gram
ghci> let room_temperature = 491.0      *~ degreeRankine
ghci> let velocity         = 777        *~ knot
ghci>
ghci> (maxwellBoltzmannDist n2_mass room_temperature velocity) /~ ((hour / nauticalMile) ^ pos3)
5.041390507268275e-12
```

## Design and tradeoffs

_This section was added on Nov. 24th, 2024 following discussions on [Hacker News](https://news.ycombinator.com/item?id=42202834) and the [Haskell Discourse](https://discourse.haskell.org/t/blog-post-scientific-computing-with-confidence-using-typed-dimensions/10767?u=laurentrdc)_

There are many other software packages that support some level of type-safe computational unit systems such as [mp-units for C++](https://github.com/mpusz/mp-units), [Physics::Measure and Physics::Unit for Raku](https://raku.land/zef:librasteve/Physics::Measure), [Unitful for Julia](https://github.com/PainterQubits/Unitful.jl), [Physical for Swift](https://github.com/hyperjeff/Physical), and more. There's even an entire programming language designed around it called [Numbat](https://github.com/sharkdp/numbat). So what can we learn from `dimensional`'s design, and importantly, its tradeoffs?

#### Beyond checking to inference of dimensions

One key feature of `dimensional` is that it does not only _check validity_, but also __infers__ the dimensions of expressions at compile-time. Take for example, consider the question "What is the type (`:t`) of 1 meter divided by 1 second?":

```haskell
ghci> :t (1 *~ meter) / (1 *~ second)
Quantity (Dim Pos1 Zero Neg1 Zero Zero Zero Zero) Double
```

If you read the documentation for [`Dimension`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#t:Dimension), you will notice that `(Dim Pos1 Zero Neg1 Zero Zero Zero Zero)` is the dimension of velocity (also known as [`DVelocity`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Quantities.html#t:DVelocity)).

This means that the compiler becomes a helpful assistant, rather than simply telling you when you are wrong.

#### Compile-time dimensions, runtime units

Another key design of `dimensional` is that the dimensions are typed, but units are runtime values. This has important advantage, but also one major drawback.

The important advantage is flexibility: programs written using `dimensional` accept a wide variety of inputs whose dimensions are equivalent. Since values' dimensions are known at compile-time, we can be guaranteed that values will be convertible to base units at runtime. You can see this in [the usage of `maxwellBoltzmannDist` above](#interactive-example), where I varied the input units without issues, even mixing SI units like `kelvin` and non-SI units like `degreeRankine`.

This flexibility comes with a performance cost: each quantity is now composed of two values, the magnitude and the unit, which means that that mathematical operations will be slower. For performance-sensitive code, I recommend validating inputs using `dimensional`, and converting to raw `Double` using [`(/~)`](https://hackage.haskell.org/package/dimensional-1.6.1/docs/Numeric-Units-Dimensional-Prelude.html#v:-47--126-) before compute-heavy operations. 

#### Non-extensibility

`dimensional` is limited to the 7 physical dimensions by design. However, other libraries such as [units](https://hackage.haskell.org/package/units), allow the creation of extensible unit systems, not limited to physical dimensions.

The lack of extensibility is not an inherent design choice, but rather an implementation choice. This may change in the future.

## Conclusion

This was a whirlwind tour of `dimensional` with a practical example. If this was your first introduction to typed dimensions in Haskell, do not be alarmed -- I bounced off of it the first time.

As mentioned, software design is about trade-off. Using `dimensional` is complex, but it isn't complicated[^complex]; it makes you address the complexity of dimensions and units up-front, rather than hoping for the best. Sometimes this doesn't make sense, but for critical calculations, there is nothing like typed dimensions.

I am now a [maintainer of `dimensional` on GitHub](https://github.com/bjornbm/dimensional/), and __I want nothing more than to help people use typed dimensions__ (when it makes sense). If you have any feedback, for example missing features or lacking documentation, [feel free to raise an issue](https://github.com/bjornbm/dimensional/issues)!

[^complex]: [Complexity is inherent, complicatedness is extraneous](https://english.stackexchange.com/questions/10459/what-is-the-difference-between-complicated-and-complex). 

*Code available in [Haskell module](/files/typed-dimensions/TypedDimensions/Typed.hs).*