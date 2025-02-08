---
title: "When one temperature is not enough: the two-temperature model"
date: 2019-04-03
summary: What if we could prepare a system with two distinct temperatures? What would it look like?
tags: science
---

Temperature is a measure of the average kinetic energy of all particles in a system. An example of such as system is presented below:

![Translational motion of particles in a box. Some particles are colored red for better tracking.  [Image credit to A. Greg.](https://en.wikipedia.org/wiki/Thermodynamic_temperature#/media/File:Translational_motion.gif)](/images/Translational_motion.gif)

Note that the above system has a temperature because there exists a clear **average** motion, even though not all particles are moving at the same velocity. This means, a system is at some temperature $T$ as long as the distribution of kinetic energies (often related to velocities) ressembles a **normal distribution**:

```{.matplotlib caption="Examples of distribution of particle kinetic energies. **Left**: distribution of particle energies with a well-defined temperature. **Right**: distribution of particle energies does not match an expected thermal equilibrium."}

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
    ax.set_yticks([])
    ax.set_xticks([])
```

So, a system with a well-defined temperature exhibits a normal distribution of particle energies. It turns out that it is possible to prepare systems into a state where there are *two clear average energies* , if only for a very short moment. 

Real materials are composed of two types of particles, nuclei and electrons[^1]. These particles have widly different masses, so electromagnetic fields --- for example, an intense pulse of light --- will not affect them at the same time; since nuclei are at least ~1000x more massive than electrons, we should expect the electrons to react about ~1000x faster.

After decades of development culminating in the 2018 Nobel Prize in Physics, the production of ultrafast laser pulses (less than 30 femtoseconds[^2]) is now routine. These ultrafast laser pulses can be used to prepare systems in a strange configuration: one with seemingly *two temperatures*, albeit only for a short time. Modeling of this situation in crystalline material was done decades ago, and the model is known as the **two-temperature model**[^3].

Roughly 100fs after dumping a lot of energy into a material, the nuclei might not have reacted yet, and we might have the following energetic landscape:

```{.matplotlib caption="Idealized view of the distribution of kinetic energy, 100 femtosecond after photoexcitation by an ultrafast laser pulse. For a very short time, the system can be described by two temperatures; one for the lattice of nuclei, $T_l$, and one for the electronic system, $T_e$."}

CENTER1 = 4.0
CENTER2 = CENTER1 * 1/4

electrons = np.random.normal(loc=CENTER1, scale=0.2, size=3000)
lattice = np.random.normal(loc=CENTER2, scale=0.5, size=2500)

fig, ax = plt.subplots(1,1, figsize=(8,3))

ax.axvline(x=CENTER1, color='k')
ax.axvline(x=CENTER2, color='k')

electrons_hist, *_ = ax.hist(electrons, bins=50, color='b', label='electrons')
lattice_hist, *_ = ax.hist(lattice, bins=50, color='r', label='nuclei')

textheight = min([electrons_hist.max(), lattice_hist.max()])/4

ax.text(x=1.02*CENTER1, y=textheight, s="$T_e$")
ax.text(x=1.02*CENTER2, y=textheight, s="$T_l$")

ax.set_xlabel('Particle kinetic energy')
ax.set_ylabel('Number of particles')
ax.set_yticks([])
ax.set_xticks([])
plt.legend()
```

where the nucliei will still be at equilibrium temperature, and the electrons might be at a temperature of 20000$^{\circ}$C. Therefore, *we have a system with two temperatures* for a few picoseconds[^4].


[^1]: The atomic forces at the nanometer-scale are mostly electromagnetic, so I will consider the atomic nuclei as a single particle.

[^2]: $1$ femtosecond $= 10^{-15}$ seconds

[^3]: P. B. Allen, *Theory of thermal relaxation of electrons in metals* (1987). Physics Review Letters **59**, DOI: [10.1103/PhysRevLett.59.1460](https://link.aps.org/doi/10.1103/PhysRevLett.59.1460)

[^4]: $1$ picosecond $= 1000$ fs $= 10^{-12}$ seconds