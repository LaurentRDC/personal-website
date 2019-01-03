---
title: The intuition and beauty behind integral transforms
date: 2019-01-23
summary: Integral transforms (such as the Fourier transform) are fundamental tools of science and engineering. However, I find that many lack any intuition about it. Let's change this.
---

The Fourier transform is a ubiquitous tool in Physics and related fields of science. Using it, we can represent physical systems in a complementary way -- often in a simpler way as well! Throughout the years, I've noticed that while most students have mastered the technicals required to _compute_ the Fourier transform, many lack the intuition of what's going on behind the scenes.

In this post, I would like to explain how I think about the Fourier transform. I will also lay the foundation required to talk about other integral transforms as well, like the Laplace transform, or the lesser-known wavelet transform.

## Separating objects into building blocks in vector spaces

We begin our journey in the land of __linear algebra__. The first time students are exposed to linear algebra, they are exposed to the set of points in 2D, called $\mathbf{R}^2$. Intuitively, this can also be considered the set of arrows in a plane. Hopefully, you also remember the following property : in $\mathbf{R}^2$, every vector (i.e. every arrow) can be decomposed into two arrows: one horizontal arrow and one vertical arrow. Mathematically, any arrow $\mathbf{r}$ can be decomposed as follows:
$$
    \mathbf{r} = x ~ \hat{\mathbf{e}}_1 + y ~ \hat{\mathbf{e}}_2
$$
In this case, $\hat{\mathbf{e}}_1$ is a horizontal arrow of length 1, and  $\hat{\mathbf{e}}_2$ of length 1. Graphically, we can represent this situation like this:

```{plot_target=generated/r2_arrows.png plot_alt="Decomposing any arrow in the place consist in a scaled vertical arrow, and a scaled horizontal arrow."}
import matplotlib.pyplot as plt

fig, ax1 = plt.subplots()

arrow_kwargs = {'length_includes_head': True,
                'width': 0.05,
                'capstyle': 'round'}
text_kwargs = {'fontsize': 20}
x, y = 3, 4

ax1.arrow(0, 0, x, y, color = 'k', **arrow_kwargs)
ax1.text(2*x/3 - 0.1, 2*y/3 + 0.1, r'$\mathbf{r}$', **text_kwargs)

ax1.arrow(0, 0, x, 0, color = 'r', **arrow_kwargs)
ax1.text(x/2, 0.1, r'$x \hat{\mathbf{e}}_1$', color = 'r', **text_kwargs)
ax1.axvline(x = x, linestyle = '--', color = 'r')

ax1.arrow(0, 0, 0, y, color = 'b', **arrow_kwargs)
ax1.text(0.1, y/2, r'$y \hat{\mathbf{e}}_2$', color = 'b', **text_kwargs)
ax1.axhline(y = y, linestyle = '--', color = 'b')

ax1.xaxis.set_visible(False)
ax1.yaxis.set_visible(False)

ax1.set_xlim([-0.2, x + 0.2])
ax1.set_ylim([-0.2, y + 0.2])
```

The decomposition of an arrow $\mathbf{r}$ therefore involves finding the right proportion of $\hat{\mathbf{e}}_1$ and $\hat{\mathbf{e}}_2$. We can also think of it as scaling (i.e. stretching) $\hat{\mathbf{e}}_1$ and $\hat{\mathbf{e}}_2$ by $x$ and $y$, respectively; this is why $x$ and $y$ are called __scalars__!

The fundamental building blocks of arrows (or vectors in $\mathbf{R}^2$), $\hat{\mathbf{e}}_1$ and $\hat{\mathbf{e}}_2$, are called __basis vectors__. We can construct any arrow adding the basis vectors $\hat{\mathbf{e}}_1$ and $\hat{\mathbf{e}}_2$ in the right proportions [^1]. Are the basis vectors unique? No! We can use different bases if they are convenient. Let's create a new basis[^2]:

$$
\begin{align}
    \hat{\mathbf{u}} &= \frac{\hat{\mathbf{e}}_1 + \hat{\mathbf{e}}_2}{\sqrt{2}} \\
    \hat{\mathbf{v}} &= \frac{\hat{\mathbf{e}}_1 - \hat{\mathbf{e}}_2}{\sqrt{2}}
\end{align}
$$

In certain situations, using basis vectors $\left\{ \hat{\mathbf{u}}, \hat{\mathbf{v}} \right\}$ might be more appropriate than using $\left\{ \hat{\mathbf{e}}_1, \hat{\mathbf{e}}_2 \right\}$. For examples, an arrow $\mathbf{a}$ of length $L$ at 45&deg; has a simple representation in the basis $\left\{ \hat{\mathbf{u}}, \hat{\mathbf{v}} \right\}$: $\mathbf{a} = L ~ \hat{\mathbf{u}}$. Depending on your the problem at hand, changing the basis might simplify problems a lot! Here's a graphical representation of what I'm talking about:

```{plot_target=generated/new_basis_uv.png plot_alt="Comparison of two possible bases for arrows in the plane, effectively rotated by 45&deg;."}
import matplotlib.pyplot as plt
from math import sqrt

sqrt2 = sqrt(2)
arrow_kwargs = {'length_includes_head': True,
                'width': 0.05,
                'capstyle': 'round'}
text_kwargs = {'fontsize': 20}

fig, ax1 = plt.subplots(figsize = (4,4))

ax1.arrow(0, 0, 1, 0, color = 'r', **arrow_kwargs)
ax1.text(1.05, 0, r'$\hat{\mathbf{e}}_1$', color = 'r', **text_kwargs)

ax1.arrow(0, 0, 0, 1, color = 'b', **arrow_kwargs)
ax1.text(0, 1.05, r'$\hat{\mathbf{e}}_2$', color = 'b', **text_kwargs)

ax1.arrow(0, 0, 1/sqrt2, 1/sqrt2, color = 'g', **arrow_kwargs)
ax1.text(1/2, 0.7, r'$\hat{\mathbf{u}}$', color = 'g', **text_kwargs)

ax1.arrow(0, 0, 1/sqrt2, -1/sqrt2, color = 'y', **arrow_kwargs)
ax1.text(1/2, -0.4, r'$\hat{\mathbf{v}}$', color = 'y', **text_kwargs)

ax1.xaxis.set_visible(False)
ax1.yaxis.set_visible(False)

ax1.set_xlim([-0.8, 1.3])
ax1.set_ylim([-0.8, 1.3])
```

One more thing: the scaling of the basis vectors ($x$ and $y$ in the case of $\mathbf{r} = x ~ \hat{\mathbf{e}}_1 + y ~ \hat{\mathbf{e}}_2$) can be referred to _projections_ of $\mathbf{r}$ onto the basis vectors. You might have seen this before: the action of projecting $\mathbf{r}$ onto either $\hat{\mathbf{e}}_1$ or $\hat{\mathbf{e}}_2$ is called the __inner product__ $(\cdot)$:
$$
    \mathbf{r} = (\overbrace{\mathbf{r} \cdot \hat{\mathbf{e}}_1}^{x}) ~ \hat{\mathbf{e}}_1 + (\underbrace{\mathbf{r} \cdot \hat{\mathbf{e}}_2}_{y}) ~ \hat{\mathbf{e}}_2
$$
Therefore, if we want to know "how much" of $\hat{\mathbf{e}}_1$ is in $\mathbf{r}$, we have $\mathbf{r} \cdot \hat{\mathbf{e}}_1 = x$.

Let's recapitulate the properties of our simple example of $\mathbf{R}^2$:

1. Arrows (vectors) can be decomposed into basic building blocks, called __basis vectors__, that are stretched by __scalars__ to the appropriate length;

2. The proportion of each basis vector in an arrow is the __projection__ of the arrow onto the basis vector, denoted with the inner product $(\cdot)$;

3. Sets of basis vectors, like $\left\{ \hat{\mathbf{e}}_1, \hat{\mathbf{e}}_2 \right\}$, are not unique;

While I explained these properties for arrows, these properties hold for many types of vectors. There are some exotic things that follow these properties...

### Spaces of functions

_What if I told you that you can decompose functions into basis functions?_

It turns out[^3] that that well-behaved functions have similar properties to arrows. Most importantly, there exists many sets of functions (_basis functions_) that can be used to decompose nice functions.

Like in the example above, sets of _basis functions_ are not unique. Different sets of basis functions help with different kinds of problem. Let's consider a very special set of basis functions: sines and cosines

## The Fourier transform

The Fourier transform is a tool to decompose well-behaved functions into a set of basis functions that oscillate like a wave. Here are a few basis functions used in the Fourier transform:

```{plot_target=generated/fourier_basis_ex.png plot_include=images/plot_style.py  plot_alt="Example of basis functions employed by the Fourier transform. The full set of basis functions is actually infinite, but all functions follow this template."}
import matplotlib.pyplot as plt
import numpy as np

time = np.linspace(-5, 5, 1024)

fig, ax = plt.subplots()
ax.set_xlabel('Time [s]')
ax.yaxis.set_visible(False)

ax.plot(time, np.sin(np.pi*time) + 2.5)
ax.plot(time, np.sin(np.pi*time/2 + 1))
ax.plot(time, np.sin(2*np.pi*time - np.pi) - 2.5)
```

The set of basis functions for the Fourier transform is the set of all oscillatory functions with a single, well-defined frequency $k$[^4]:
$$
    \left\{  e^{2\pi i x k} \right\}
$$
There are an infinite number of basis vectors in this basis, but each basis vector can be labeled with one number: its frequency. To get a feel for what frequency does, let's take a look at a few basis functions:

```{plot_target=generated/fourier_basis_freq.png plot_include=images/plot_style.py plot_alt="Effect of frequency on basis functions. Higher frequency effectively compresses the wave into faster oscillations."}
import matplotlib.pyplot as plt
import numpy as np

time = np.linspace(-5, 5, 1024)
frequencies = [1/2, 1, 2, 5]

fig, ax = plt.subplots()
ax.set_xlabel('Time [s]')
ax.yaxis.set_visible(False)
ax.set_ylim([-3*len(frequencies) + 1, 2])

for index, freq in enumerate(frequencies):
    offset = index * 3
    ax.plot(time, np.sin(2*np.pi*freq*time) - offset)
    ax.text(0, -offset+1.5, 'Frequency = {}Hz'.format(freq),
            horizontalalignment='center',
            verticalalignment='center',
            fontsize=12)

# time = np.linspace(0, 5, 1024)
# phases = [0, np.pi/2, np.pi, 3*np.pi/2, 2*np.pi]

# fig, ax = plt.subplots()
# ax.set_xlabel('Time [s]')
# ax.yaxis.set_visible(False)
# ax.set_ylim([-3*len(phases) + 1, 2])

# for index, phase in enumerate(phases):
#     offset = index * 3
#     ax.plot(time, np.sin(2*np.pi*time + phase) - offset)
#     ax.text(2.5, -offset+1.5, 'Phase = {:.1f} $\\pi$ rads'.format(phase/np.pi),
#             horizontalalignment='center',
#             verticalalignment='center',
#             fontsize=12)
```

Because there are an infinite number of Fourier basis functions, the decomposition of any function $f$ into the Fourier basis involves an integral rather than a sum:

$$
    f(x) = \int_{-\infty}^{\infty} dk ~ \hat{f}(k) e^{2\pi i x k}
$$

but conceptually, it is the same as the addition of arrows. Therefore, the Fourier transform basis is a set of oscillating functions that can be compressed or elongated (varying frequency $k$). The "amount" of $f$ at any given frequency $k$, $\hat{f}(k)$, is a projection $(\cdot)$, again just like in our arrow example. The projection operator, $f ~ \cdot ~ e^{2 \pi i x k}$, gives rise to the operation that is formally known as the Fourier transform:
$$
    \hat{f}(k) = \int_{-\infty}^{\infty} dx ~ f(x) ~ e^{- 2 \pi i x k}
$$

To summarize what we have seen:

1. The Fourier basis is a basis of oscillating functions, with general form $e^{2 \pi i x k}$;

2. The value of $\hat{f}(k)$ gives you the "amount" of $e^{2 \pi i x k}$ in $f(x)$;

3. The calculation of this "amount", formally the projection of $f(x)$ onto $e^{2 \pi i x k}$, is called the Fourier transform.

## Amplitude and Phase

[^1]: This fact might seem obvious, but the general proof that every vector space has a basis involves the [axiom of choice](https://en.wikipedia.org/wiki/Axiom_of_choice).

[^2]: I'm using the convention where basis vectors with a hat $(\hat{})$ have length 1. This is why $\hat{\mathbf{u}}$ and $\hat{\mathbf{v}}$ are normalized by $\sqrt{2}$.

[^3]: The spirit of this post is to develop an intuition regarding integral transforms. Therefore, I'm skipping over the details of function spaces like $\mathbf{L}^2$.

[^4]: If you are unfamiliar with the imaginary unit $i$, you can think think of $e^{2 \pi i k x} = \cos (2 \pi x k) + i \sin(2 \pi x k)$.