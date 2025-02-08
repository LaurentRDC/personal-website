---
title: Filtering noise with discrete wavelet transforms
date: 2022-11-23
summary: Data is often noisy, containing a meaningful signal component as well as undesired perturbations. Many filtering techniques can be used to filter away noise. In this post, we'll see how to use discrete wavelet transforms to de-noise data. 
withtoc: yes
tags: science, mathematics, python
---

All experimental data contains noise. Distinguishing between measurement and noise is an important component of any data analysis pipeline. However, different noise-filtering techniques are suited to different categories of noise.

In this post, I'll show you a class of filtering techniques, based on discrete wavelet transforms, which is suited to noise that cannot be filtered away with more traditional techniques -- such as ones that rely on the Fourier transform. This has been important in my past research[^baseline] [^vo2], and I hope that this can help you too.

[^baseline]: __L. P. René de Cotret__ and B. J. Siwick, _A general method for baseline-removal in ultrafast electron powder diffraction data using the dual-tree complex wavelet transform_, Struct. Dyn. __4__ (2017) [DOI:10.1063/1.4972518](http://scitation.aip.org/content/aca/journal/sdy/4/4/10.1063/1.4972518)

[^vo2]: M. R. Otto, __L. P. René de Cotret__, *et al*, _How optical excitation controls the structure and properties of vanadium dioxide_, PNAS (2018) [DOI: 10.1073/pnas.1808414115](https://doi.org/10.1073/pnas.1808414115).

## Integral transforms

A large category of filtering techniques are based on *integral transforms*. Broadly speaking, an integral transform $T$ is an operation that is performed on a function $f$, and builds a new function $T\left[ f\right]$ which is defined on a variable $s$, such that:

$$
    T\left[ f \right](s) = \int dt ~ f(t) \cdot K(t, s)
$$

Here, $K$ (for kernel) is a function which "selects" which parts of $f(t)$ are important at a fixed $s$. Note that for an integral transform to be useful as a filter, we'll need the ability to invert the transformation, i.e. there exists an inverse kernel $K^{-1}(s, t)$ such that:

$$
    f(t) = \int ds ~ \left( T \left[ f\right] (s) \right) \cdot K^{-1}(s,t)
$$

All of this was very abstract, so let's look at a concrete example: the Fourier transform. The Fourier transform is an integral transform where[^3]:

$$
    \begin{align}
        K(t, \omega)      &\equiv \frac{e^{-i \omega t}}{\sqrt{2 \pi}}\\
        K^{-1}(\omega, t) &\equiv  e^{i \omega t}\\
        \omega            & \in \mathbb{R}
    \end{align}
$$

[^3]: Note that it is traditional in physics to represent the transform variable as $\omega$ instead of $s$. If $t$ is time (in seconds), then $\omega$ is *angular frequency* (in radians per seconds). If $t$ is distance (in meters), $\omega$ is spatial angular frequency (in radians per meter).

There are many other integral transforms, such as:

* The Laplace transform ($K(t, s) \equiv e^{- s t}$), which is useful to solve linear ordinary differential equations;
* The Legendre transforms ($K_n(t, s) \equiv P_n(s)$, where $P_n$ is the n^th^ [Legendre polynomial](https://en.wikipedia.org/wiki/Legendre_polynomials)) which is used to solve for electron motion in hydrogen atoms;
* The Radon transform (for which I cannot write down a kernel), which is used to analyze [computed tomography data](https://scikit-image.org/docs/stable/auto_examples/transform/plot_radon_transform.html). 

So why are integral transforms interesting? Well, depending on the function $f(t)$ you want to transform, you might end up with a representation of $f$ in the transformed space, $T \left[ f\right] (s)$, which has nice properties! Re-using the Fourier transform for a simple, consider a function made up of two well-defined frequencies:

$$
    f(t) \equiv e^{-i ~ 2t} + e^{-i ~ 5t}
$$

The representation of $f(t)$ in frequency space -- the Fourier transform of $f$, $F\left[ f\right](\omega)$ -- is very simple:

$$
    F\left[ f\right](\omega) = \sqrt{2 \pi} \left[ \delta(\omega - 2) + \delta(\omega - 5) \right]
$$

The Fourier transform of $f$ is perfectly localized in frequency space, being zero everywhere except at $\omega=2$ and $\omega=5$. Functions composed of infinite waves (like the example above) always have the nice property of being localized in frequency space, which makes it easy to manipulate them... like filtering some of their components away!

### Discretization

It is much more efficient to use discretized versions of integral transforms on computers. Loosely speaking, given a discrete signal composed of $N$ terms $x_0$, ..., $x_{N-1}$:

$$
    T\left[ f \right](k) = \sum_n x_n \cdot K(n, k)
$$

i.e. the integral is now a finite sum. For example, the discrete Fourier transform of the signal $x_n$, $X_k$, can be written as:

$$
    X_k = \sum_n x_n \cdot e^{-i 2 \pi k n / N}
$$

and its inverse becomes:

$$
    x_n = \frac{1}{N}\sum_k X_k \cdot e^{i 2 \pi k n / N}
$$

This is the definition used by [numpy](https://numpy.org/doc/stable/reference/routines.fft.html#module-numpy.fft). Let's use this definition to compute the discrete Fourier transform of $f(t) \equiv e^{-i ~ 2t} + e^{-i ~ 5t}$:

```{.python .matplotlib caption="**Top**: Signal which is composed of two natural frequencies. **Bottom**: Discrete Fourier transform of the top signal, showing two natural frequencies."}
import numpy as np
import matplotlib.pyplot as plt

t = np.linspace(-1, 1, 1024)
f = np.exp(1j * 2 * 2 * np.pi * t) + np.exp(1j * 2 * np.pi * 5 * t)

# For technical reasons, the frequency components of the Fourier transform
# are not arranged as one would expect. In order for the plot to look OK, we
# need to use `np.fft.fftshift`
fhat = np.fft.fftshift(np.fft.fft(f))
w = np.fft.fftshift(np.fft.fftfreq(f.shape[0], d=t[1] - t[0]))

fig, (ax_t, ax_w) = plt.subplots(2, 1, figsize=(8, 6))

ax_t.plot(t, np.real(f), color="k")
ax_t.set_xlim([t.min(), t.max()])
ax_t.set_xlabel("Time [s]")

ax_w.plot(w, np.abs(fhat)**2, ".", color="k")
ax_w.set_xlim([-2, 9])
ax_w.set_ylabel("Spectral power [a.u.]")
ax_w.set_xlabel("Angular frequency [rad/s]")

for freq in [2, 5]:
    ax_w.axvline(x=freq, linestyle='--', linewidth=1, color='k')
    ax_w.annotate(f"$\omega={freq}$", xy=(1.05*freq, 0.95*np.square(np.abs(fhat)).max()), xycoords='data')

```

## Using the discrete Fourier transform to filter noise

Let's add some noise to our signal and see how we can use the discrete Fourier transform to filter it away. The discrete Fourier transform is most effective if your noise has some nice properties in frequency space. For example, consider high-frequency noise:

$$
    N(t) = \sum_{\omega=20}^{50} \sin(\omega t + \phi_{\omega})
$$

where $\phi_\omega$ are random phases, one for each frequency component of the noise. While the signal looks very noisy, it's very obvious in frequency-space what is noise and what is signal:
```{.python .matplotlib caption="**Top**: Noisy signal (red) with the pure signal shown in comparison. **Bottom**: Discrete Fourier transform of the noisy signal shows that noise is confined to a specific region of frequency space."}
import numpy as np
import matplotlib.pyplot as plt
from random import random, seed

seed(2022)

t = np.linspace(-1, 1, 1024)
f = np.exp(1j * 2 * 2 * np.pi * t) + np.exp(1j * 2 * np.pi * 5 * t)

noise = np.zeros_like(f)
for freq in range(20, 50):
    phase = random()
    amplitude = random()
    noise += amplitude * np.sin(2*np.pi*freq*t + phase)

# For technical reasons, the frequency components of the Fourier transform
# are not arranged as one would expect. In order for the plot to look OK, we
# need to use `np.fft.fftshift`
fhat = np.fft.fftshift(np.fft.fft(f + noise))
w = np.fft.fftshift(np.fft.fftfreq(f.shape[0], d=t[1] - t[0]))

fig, (ax_t, ax_w) = plt.subplots(2, 1, figsize=(8, 6))

ax_t.plot(t, np.real(f)+noise, color='r', label="Noisy signal")
ax_t.plot(t, np.real(f), color="k", label="Pure signal")
ax_t.set_xlim([t.min(), t.max()])
ax_t.set_xlabel("Time [s]")
ax_t.legend()

ax_w.plot(w, np.abs(fhat)**2, ".", color="k")
ax_w.set_xlim([-2, 55])
ax_w.set_ylabel("Spectral power [a.u.]")
ax_w.set_xlabel("Angular frequency [rad/s]")

for freq in [2, 5]:
    ax_w.axvline(x=freq, linestyle='--', linewidth=1, color='k')

ymin, ymax = ax_w.get_ylim()
xmin, xmax = ax_w.get_xlim()
ax_w.fill_betweenx(y=ax_w.get_ylim(), x1=20, x2=50, alpha=0.1, color='r')
ax_w.text(x=(50-20)/2 + 20, y=(ymax - ymin)/2, s="Noise region", color='r', va='center', ha='center')
ax_w.set_ylim([ymin, ymax])
```

The basics of filtering is as follows: set the transform of a signal to 0 in regions which are thought to be undesirable. In the case of the Fourier transform, this is known as a *band-pass filter*; frequency components of a particular frequency *band* are passed-through unchanged, and frequency components outside of this band are zeroed. Special names are given to band-pass filters with no lower bound (low-pass filter) and no upper bound (high-pass filter). We can express this filtering as a window function $W_k$ in the inverse discrete Fourier transform:

$$
    x_{n}^{\text{filtered}} = \frac{1}{N}\sum_k W_k \cdot X_k \cdot e^{i 2 \pi k n / N}
$$

In the case of the plot above, we want to apply a low-pass filter with a cutoff at $\omega=10$. That is:

$$
W_k = \left\{ \begin{array}{cl}
    1 & : \ |k| \leq 10 \\
    0 & : \ |k| > 10
\end{array} \right.
$$

Visually:

```{.python .matplotlib caption="**Top**: Noisy signal with the pure signal shown in comparison. **Middle**: Discrete Fourier transform of the noisy signal. The band of our band-pass filter is shown, with a cutoff of $\omega=10$. All Fourier components in the zeroed region are set to 0 before performing the inverse discrete Fourier transform. **Bottom**: Comparison between the filtered signal and the pure signal. The only (small) deviations can be observed at the edges."}
import numpy as np
import matplotlib.pyplot as plt
from random import random, seed

seed(2022)

t = np.linspace(-1, 1, 1024)
f = np.exp(1j * 2 * 2 * np.pi * t) + np.exp(1j * 2 * np.pi * 5 * t)

noise = np.zeros_like(f)
for freq in range(20, 50):
    phase = random()
    amplitude = random()
    noise += amplitude * np.sin(2*np.pi*freq*t + phase)

# For technical reasons, the frequency components of the Fourier transform
# are not arranged as one would expect. In order for the plot to look OK, we
# need to use `np.fft.fftshift`
fhat = np.fft.fftshift(np.fft.fft(f + noise))
w = np.fft.fftshift(np.fft.fftfreq(f.shape[0], d=t[1] - t[0]))

window = np.ones_like(w)
window[np.logical_or(w > 10, w < -10)] = 0
filtered = np.fft.ifft(np.fft.ifftshift(window*fhat))

fig = plt.figure(figsize=(8,9))
ax_t = plt.subplot(3,1,1)
ax_w = plt.subplot(3,1,2)
ax_filtered = plt.subplot(3,1,3, sharex=ax_t)

ax_t.plot(t, np.real(f)+noise, color='r', label="Noisy signal")
ax_t.plot(t, np.real(f), color="k", label="Pure signal")
ax_t.set_xlim([t.min(), t.max()])
ax_t.set_xlabel("Time [s]")
ax_t.legend()

# Fourier transform

ax_w.plot(w, np.abs(fhat)**2, ".", color="k")
ax_w.set_xlim([-2, 55])
ax_w.set_ylabel("Spectral power [a.u.]")
ax_w.set_xlabel("Angular frequency [rad/s]")

ax_w.axvline(x=10, linestyle='--', linewidth=1, color='k')
ax_w.annotate(f"Cutoff", xy=(1.05*10, 0.95*np.square(np.abs(fhat)).max()), xycoords='data')

ymin, ymax = ax_w.get_ylim()
xmin, xmax = ax_w.get_xlim()
ax_w.fill_betweenx(y=[ymin, ymax], x1=-2, x2=10, alpha=0.1, color='g')
ax_w.text(x=4, y=(ymax - ymin)/2, s="Band pass", color='g', va='center', ha='center')
ax_w.fill_betweenx(y=[ymin, ymax], x1=10, x2=55, alpha=0.1, color='r')
ax_w.text(x=(xmax-10)/2 + 10, y=(ymax - ymin)/2, s="Zeroed region", color='r', va='center', ha='center')
ax_w.set_ylim([ymin, ymax])

# Final output

ax_filtered.plot(t, np.real(f), color='k', label="Pure signal")
ax_filtered.plot(t, filtered, color="g", label="Filtered signal")
ax_filtered.set_xlim([t.min(), t.max()])
ax_filtered.set_xlabel("Time [s]")
ax_filtered.legend()
```

The lesson here is that filtering signals using a discretized integral transform (like the discrete Fourier transform) consists in:

1. Performing a forward transform;
2. Modifying the transformed signal using a window function, usually by zeroing components;
3. Performing the inverse transform on the modified signal.

## Discrete wavelet transforms

Discrete wavelet transforms are a class of discrete transforms which decomposes signals into a sum of wavelets. While the complex exponential functions which make up the Fourier transform are localized in frequency but infinite in space, wavelets are localized in both time space and frequency space. 

In order to generate the basis wavelets, the original wavelet is stretched. This is akin to the Fourier transform, where the sine/cosine basis functions are 'stretched' by decreasing their frequency. In technical terms, the amount of 'stretch' is called the **level**. For example, the discrete wavelet transform using the `db4`[^pywt] wavelet up to level 5 is the decomposition of a signal into the following wavelets:

[^pywt]: I will be using the wavelet naming scheme from [PyWavelets](https://pywavelets.readthedocs.io/en/latest/ref/index.html).

```{.python .matplotlib caption="Five of the `db4` basis wavelets shown. As the level increases, the wavelet is stretched such that it can represent lower-frequency components of a signal."}
import matplotlib.pyplot as plt
import pywt

fig, ax = plt.subplots(figsize=(8,4))

def discrete_colors(it):
    """Returns a list of discrete colors to plot, for example, various time-traces."""
    cmap = plt.get_cmap("inferno")
    mi, ma = 0.11, 0.75

    it = list(it)

    if len(it) == 1:
        yield from zip([cmap(mi)], it)
        return
    elif len(it) == 2:
        yield from zip([cmap(mi), cmap(ma)], it)
        return

    step = (ma - mi) / (len(it) - 1)
    yield from zip([cmap(mi + i * step) for i in range(len(it))], it)

offset = 0
for color, level in discrete_colors([1,2,3,4,5]):
    _, wavelet, _ = pywt.Wavelet("db4").wavefun(level=level)
    
    ax.plot(wavelet + offset, color=color, label=f"level={level}")
    offset -= 2

ax.legend()
ax.grid(True)
ax.set_ylabel('Amplitude [a.u.]')
plt.yticks(color='w') # Hiding ytick labels, which are meaningless due to the offset.
ax.set_xlabel('Time [a.u.]')
```

In practice, discrete wavelet transforms are expressed as two transforms per level. This means that a discrete wavelet transform of level 1 gives back two sets of coefficients. One set of coefficient contains the low-frequency components of the signal, and are usually called the *approximate coefficients*. The other set of coefficients contains the high-frequency components of the signal, and are usually called the *detail coefficients*. A wavelet transform of level 2 is done by taking the approximate coefficients of level 1, and transforming them using a stretched wavelet into two sets of coefficients: the approximate coefficients of level 2, and the detail coefficients of level 2. Therefore, a signal transformed using a wavelet transform of level $N$ has $N$ sets of coefficients: the approximate and detail coefficients of level $N$, and the detail coefficients of levels $N-1$, $N-2$, ..., $1$.

## Filtering using the discrete wavelet transform

The discrete Fourier transform excels at filtering away noise which has nice properties in frequency space. This is isn't always the case in practice; for example, noise may have frequency components which overlap with the signal we're looking for. This was the case in my research on ultrafast electron diffraction of polycrystalline samples[^baseline] [^vo2], where the 'noise' was a trendline which moved over time, and whose frequency components overlapped with diffraction pattern we were trying to isolate.

As an example, let's use [real electron diffraction data](/files/wavelet-filter/diffraction.csv) and we'll pretend this is a time signal, to keep the units familiar. We'll take a look at some really annoying noise: normally-distributed white noise drawn from this distribution[^bias]:

$$
    P(x) = \frac{1}{\sqrt{2 \pi}} \exp{-\frac{(x + 1/2)^2}{2}}
$$

[^bias]: Note that this distribution contains a bias of -$1/2$, which is useful in order to introduce low-frequencies in the noise which overlap with the spectrum of the signal.

Visually:

```{.python .matplotlib caption="**Top**: Example signal with added synthetic noise. **Bottom**: Frequency spectrum of both the pure signal and the noise, showing overlap. This figure shows that filtering techniques based on the Fourier transform would not help in filtering the noise in this signal."}
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from numpy.random import default_rng

random = default_rng(seed=2022)

data = np.loadtxt(Path("files") / "wavelet-filter" / "diffraction.csv", delimiter=",", skiprows=1)
t = data[:,0]
signal = data[:,1]

noise = random.normal(size=t.shape[0], loc=-0.5, scale=1)

fig, (ax_t, ax_w) = plt.subplots(2,1, figsize=(8,6))

ax_t.plot(t, signal + noise, color='r', label="Noisy signal")
ax_t.plot(t, signal, color='k', label="Pure signal")
ax_t.set_xlim([t[0], t[-1]])
ax_t.set_xlabel("Time [s]")
ax_t.set_ylabel("Amplitude [a.u.]")
ax_t.legend()


w = np.fft.fftshift(np.fft.fftfreq(t.shape[0], d=t[1] - t[0]))
signal_w = np.abs(np.fft.fftshift(np.fft.fft(signal)))**2
noise_w = np.abs(np.fft.fftshift(np.fft.fft(noise)))**2
noise_multiplier = 15

ax_w.plot(w, signal_w, color='k', label="Signal power spectrum")
ax_w.plot(w, noise_multiplier * noise_w, color='r', label="Noise power spectrum")
ax_w.text(x=1, y=(noise_multiplier * noise_w).max()/2, s=f"✕{noise_multiplier:0.0f}", color='r')
ax_w.set_xlim([-10, 10])
ax_w.set_xlabel("Angular frequency [rad/s]")
ax_w.set_ylabel("Spectral power [a.u.]")
ax_w.legend()
```

This example shows a common situation: realistic noise whose frequency components overlap with the signal we're trying to isolate. We wouldn't be able to use filtering techniques based on the Fourier transform.

Now let's look at a particular discrete wavelet transform, with the underlying wavelet `sym17`. Decomposing the noisy signal up to level 3, we get four components:

```{.python .matplotlib caption="All coefficients from a discrete wavelet transform up to level 3 with wavelet `sym17`."}
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pywt
from numpy.random import default_rng

def discrete_colors(it):
    """Returns a list of discrete colors to plot, for example, various time-traces."""
    cmap = plt.get_cmap("inferno")
    mi, ma = 0.11, 0.75

    it = list(it)

    if len(it) == 1:
        yield from zip([cmap(mi)], it)
        return
    elif len(it) == 2:
        yield from zip([cmap(mi), cmap(ma)], it)
        return

    step = (ma - mi) / (len(it) - 1)
    yield from zip([cmap(mi + i * step) for i in range(len(it))], it)

random = default_rng(seed=2022)

data = np.loadtxt(
    Path("files") / "wavelet-filter" / "diffraction.csv", delimiter=",", skiprows=1
)
t = data[:, 0]
signal = data[:, 1]

noise = random.normal(size=t.shape[0], loc=-0.5, scale=1)

banks = pywt.wavedec(signal + noise, pywt.Wavelet('sym17'), level=3, mode='zero')
assert len(banks) == 4

fig, axes = plt.subplots(len(banks), 1, figsize=(8,9))
for color, (ax, bank, label) in discrete_colors(zip(axes, banks, ["Approx. level=3", "Detail level=3", "Detail level=2", "Detail level=1"])):
    ax.plot(bank, color=color)
    ax.xaxis.set_visible(False)
    ax.set_title(label)
    ax.set_ylabel("Amplitude [a.u.]")
    ax.set_xlim([0, len(bank)])
```

Looks like the approximate coefficients at level 3 contain all the information we're looking for. Let's set all detail coefficients to 0, and invert the transform:

```{.python .matplotlib caption=""}
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pywt
from numpy.random import default_rng

random = default_rng(seed=2022)

data = np.loadtxt(
    Path("files") / "wavelet-filter" / "diffraction.csv", delimiter=",", skiprows=1
)
t = data[:, 0]
signal = data[:, 1]

noise = random.normal(size=t.shape[0], loc=-0.5, scale=1)

approx, *details= pywt.wavedec(signal + noise, wavelet=pywt.Wavelet('sym17'), level=3, mode='zero')
filtered = pywt.waverec([approx] + [ np.zeros_like(bank) for bank in details ], wavelet=pywt.Wavelet('sym17'), mode='zero')

fig, ax = plt.subplots(1, 1, figsize=(8,4))
ax.plot(t, signal, label="Pure signal", color='k')
ax.plot(t, filtered, label="Filtered signal", color='g')
ax.set_xlabel("Time [s]")
ax.set_ylabel('Amplitude [a.u.]')
ax.legend()
```

That's looking pretty good! Not perfect of course, which I expected because we're using real data here. 

## Conclusion

In this post, I've tried to give some of the intuition behind filtering signals using discrete wavelet transforms as an analogy to filtering with the discrete Fourier transform.

This was only a basic explanation. There is so much more to wavelet transforms. There are many classes of wavelets with different properties, some of which[^dtcwt] are very useful when dealing with higher-dimensional data (e.g. images and videos). If you're dealing with noisy data, it won't hurt to try and see if wavelets will help you understand it!

[^dtcwt]: N. G. Kingsbury, *The dual-tree complex wavelet transform: a new technique for shift invariance and directional filters*, IEEE Digital Signal Processing Workshop, DSP **98** (1998)