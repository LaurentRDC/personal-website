---
title: "When one temperature is not enough: the two-temperature model"
date: 2019-03-22
summary: What if we could prepare a system with two distinct temperatures? What would it look like?
---

Temperature is a measure of the average kinetic energy of all particles in a system. An example of such as system is presented below:

![Translational motion of particles in a box. Some particles are colored red for better tracking.  [Image credit to A. Greg](https://en.wikipedia.org/wiki/Thermodynamic_temperature#/media/File:Translational_motion.gif)](/images/Translational_motion.gif)

Note that the above system has a temperature because there exists a clear **average** motion, even though not all particles are moving at the same velocity. This means, a system is at some temperature $T$ as long as the distribution of kinetic energies (often related to velocities) ressembles a **normal distribution**:

```{.pyplot include=images/plot_style.py caption="Examples of distribution of particle kinetic energies. *Left*: distribution of particle energies with a well-defined temperature $T$. *Right*: distribution of particle energies does not match an expected thermal equilibrium. It has no well-defined temperature."}
import matplotlib.pyplot as plt
import numpy as np

np.random.seed(2019)

CENTER = 3.0

thermal = np.random.normal(loc=CENTER, scale=0.7, size=2500)
nonthermal = np.random.lognormal(mean=CENTER, sigma=0.7, size=2500)

fig, (ax_temp, ax_no_temp) = plt.subplots(1,2, figsize=(8,3))

ax_temp.axvline(x=CENTER, color='k')
thermal_hist, *_ = ax_temp.hist(thermal, bins='auto')
ax_temp.text(x=0.49, y=0.25, s="$T$", transform = ax_temp.transAxes)

nonthermal_hist, *_ = ax_no_temp.hist(nonthermal, bins='auto')
ax_no_temp.text(x=0.45, y=0.5, s="$T$?", transform = ax_no_temp.transAxes)
ax_no_temp.yaxis.set_visible(False)

for ax in (ax_temp, ax_no_temp):
    ax.set_xlabel('Particle kinetic energy')
    ax.set_ylabel('Number of particles')
    ax.set_yticks([], [])
    ax.set_xticks([], [])
```