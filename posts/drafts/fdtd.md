---
title: Design your own stealth aicraft using Yee's method
date: 2024-12-01
summary:
tags:
---

The idea here is to build software based on finite-difference time-domain (Yee's method), which allows to solve Maxwell's equations in time and space, to model the response of an aircraft to radar wave pulses.

Connect this to my experience using terahertz pulses.

Show the difference in response between basic shapes, culminating in the shape of the F-117 Nighthawk (which has a simple but effective shape)

## Electrodynamics primer

Electromagnetic waves, such as radar waves and light, are waves composed of two vector fields known as the _electric field_ and _magnetic field_. This means that everywhere in the universe, there are two vectors $\textbf{E}(\textbf{r}, t)$ and $\textbf{B}(\textbf{r}, t)$ whose magnitudes and directions must be understood.

All this to say: understanding the propagation of an electromagnetic wave __requires__ to know 6 time-varying quantities: the electric field components $\textbf{E}(\textbf{r}, t) = (E_x(t), E_y(t), E_z(t))$ and the magnetic field components $\textbf{B}(\textbf{r},t) = (B_x(t), B_y(t), B_z(t))$, over the space- and time-domain of our simulation. If you know these 6 quantities, you know everything.

Let's rip the bandaid off. Electromagnetic waves are described by a set of partial differential equations known as [Maxwell's equations](https://en.wikipedia.org/w/index.php?title=Maxwell%27s_equations&oldid=1259490248). When I said this blog post will be from first principles, the first principles are these equations[^griffiths].

[^griffiths]: Readers interested in the derivation of these equations from prior experimental and theoretical work should consider reading the _de facto_ standard undergraduate textbook _Introduction to electrodynamics_ by David J. Griffiths.

$$
\begin{align}
    \nabla \cdot \textbf{D}(\textbf{r}, t)   &= \rho_f(\textbf{r}, t)
        &&& \text{Gauss' law}\\
    \nabla \cdot \textbf{B}(\textbf{r}, t)   &= 0
         &&& \text{Gauss' law of magnetism} \\
    \nabla \times \textbf{E}(\textbf{r}, t)   &= -\frac{\partial \textbf{B}(\textbf{r}, t)  }{\partial t}
        &&& \text{Maxwell-Faraday's law}\\
    \nabla \times \textbf{H}(\textbf{r}, t)   &= \textbf{J}_f(\textbf{r}, t)   + \frac{\partial \textbf{D}(\textbf{r}, t)  }{\partial t}
        &&& \text{Amp\`ere-Maxwell's law}
\end{align}
$$

where $\textbf{H}$ is the _auxiliary magnetic field_. We use a formulation using the auxiliary field $\textbf{H}$ rather than the magnetic field $\textbf{B}$ such that we only need to worry about the free current density $\textbf{J}_f$, rather than the total current density.

There are also the constitutive equations:

$$
\begin{align}
    \textbf{D}(\textbf{r}, t)   &= \epsilon (\textbf{E}(\textbf{r}, t), \textbf{r}, t) \textbf{E}(\textbf{r}, t) \\
    \textbf{J}_f(\textbf{r}, t) &= \sigma (\textbf{E}(\textbf{r}, t), \textbf{r}, t) \textbf{E}(\textbf{r}, t) \\
    \textbf{B}(\textbf{r}, t)   &= \mu (\textbf{H}(\textbf{r}, t), \textbf{r}, t) \textbf{H}(\textbf{r}, t) \\
\end{align}
$$

where $\epsilon$, $\sigma$, and $\mu$ are the _permittivity_, _conductivity_, and _permeability_, respectively. These quantities are __properties of materials__, and as such are not considered unknown. Note that we do not assume any frequency-dependent response of materials; we consider time-dependence of material properties in the most general manner.

Recall that our goal is to solve these equations over space ($\textbf{r}$) and time ($t$) to get values of the electric field $\textbf{E}(\textbf{r}, t)$ and auxiliary magnetic field $\textbf{H}(\textbf{r}, t)$. To this end, Gauss's laws of electric and magnetic fluxes won't be directly useful to us today (although they could be used to ensure the integrity of the computation). We will instead solve Faraday's and Amp\`ere's laws iteratively, until some condition is met.

We combine the equations above to get two coupled equations for $\textbf{E}$ and $\textbf{H}$:

$$
\begin{align}
    \frac{\partial \textbf{E}}{\partial{t}} &= \frac{1}{\epsilon} \left( \nabla \times \textbf{H} \right) \\
    \frac{\partial \textbf{H}}{\partial{t}} &= -\frac{1}{\mu} \left( \nabla \times \textbf{E} \right) \\
\end{align}
$$

subject to initial and boundary conditions.

These equations can be solved in a multitude of ways, usually based on the structure of $\epsilon$, $\sigma$, and $\mu$, as well as boundary and initial conditions. For example, for a cavity composed of two planar mirrors with an initial standing wave can be solved analytically.

In general, one should solve Maxwell's equations by exploiting structures and symmetries as much as possible. This leads to more precise solutions that can be reached much quicker. There are instances, however, where many standard tools break down, and a more general approach must be taken. One such instance involves so-called __broadband sources__, the name given to pulses of light[^pulses].

[^pulses]: Short pulses of light,

In this particular post, however, I want to take a much more general approach to demonstrate a computational technique, Yee's method[^yee], which is broadly applicable.

[^yee]: K. S. Yee, *Numerical solution of initial boundary value problems involving Maxwell's equations in isotropic media*, IEEE Transactions on Antennas and Propagation **14** issue 3 (1966). [DOI:10.1109/TAP.1966.1138693](https://doi.org/10.1109/TAP.1966.1138693)

## Numerical solution using Yee's method

Time to numerically solve these equations by following , sometimes called the _finite-difference time-domain_ method. Consider a small simulation time step of value $\Delta t$. We consider the approximation to the derivative in the so-called _central difference_ scheme. Consider a general function $F$ of some one-dimensional variable $z$:

$$
    \frac{\partial \textbf{F}(z)}{\partial z}
        = \frac{ \textbf{F}(z + \Delta z) - \textbf{F}(z - \Delta z)}{2 \Delta z}
        + \mathcal{O}(\Delta z^2)
$$

where $\mathcal{O}(\Delta z^2)$ is a collection of terms that are proportional to $\Delta z^2$. Since $\Delta z$ will be chosen to be suitably small, we will be ignore terms proportional to $\Delta z^2$.

Discretizing the time derivative using the central difference scheme leads to the two update equations, one for the auxiliary magnetic field:

$$
    \textbf{H}(\textbf{r}, t + \tfrac{\delta t}{2})
        = \textbf{H}(\textbf{r}, t - \tfrac{\delta t}{2})
        - \frac{\delta t}{\mu(\textbf{r}, t)} \left( \nabla \times \textbf{E}(\textbf{r}, t) \right)
$$

and one for the electric field:

$$
    \textbf{E}(\textbf{r}, t + \delta t)
        = \textbf{E}(\textbf{r}, t)
        + \frac{\delta t}{\epsilon(\textbf{r}, t)} \left( \nabla \times \textbf{H}(\textbf{r}, t + \tfrac{\delta t}{2}) \right)
$$

where $\delta t \equiv 2 \Delta t$ is conventionally used. You will notice the __leapfrog__ time quantization; the electric field update is offset by $\delta t/2$ from the auxiliary magnetic field update. This scheme, and the space quantization scheme that will be presented below, are used to ensure that Gauss' laws of electric and magnetic fluxes are respected[^gauss].

[^gauss]: I will keep the details light in this blog post, as there are many more rigorous treatments elsewhere. The value of this blog post is getting our hands dirty.

We also quantize space, such that the fields are defined on discrete positions $\textbf{r}_{ijk} = i \Delta ~ \textbf{x} +  j \Delta ~ \textbf{y} +  k \Delta ~ \textbf{z}$, where $\textbf{x}$, $\textbf{y}$, and $\textbf{z}$ are unit vectors in the three dimensions, and $i$, $j$, $k$ are integer indices that span the size of the space we will investigate. We will switch to a different notation for discretized quantities:

$$
    \textbf{F}(\textbf{r} = i \Delta ~ \textbf{x} +  j \Delta ~ \textbf{y} +  k \Delta ~ \textbf{z}, t = n \delta t)
        \equiv \textbf{F}[i,j,k,n]
$$

Discretizing the time derivative was rather straightforward. Now, it is time to discretize terms of the form

$$
    \nabla \times \textbf{A}(\textbf{r})
$$

where $\textbf{A}$ is some vector field defined in three rectilinear dimensions. Recall the non-discretized definition:

$$
    \nabla \times \textbf{A}(\textbf{r})
        = \left( \frac{\partial A_z}{\partial y} -  \frac{\partial A_y}{\partial z} \right) \textbf{x}
        + \left( \frac{\partial A_x}{\partial z} -  \frac{\partial A_z}{\partial x} \right) \textbf{y}
        + \left( \frac{\partial A_y}{\partial x} -  \frac{\partial A_x}{\partial y} \right) \textbf{z}
$$

where $A_i$ is the component of $\textbf{A}$ in the $\textbf{i}$-direction. Again using the central difference scheme, we discretize the partial derivatives using a common step size across all three dimensions, $\Delta$:

$$
    \left( \nabla \times \textbf{A} \right)_x \approx
        \frac{1}{2 \Delta} \left( A_z[i,j+\Delta, k] - A_z[i,j-\Delta, k] - A_y[i,j,k+\Delta] + A_y[i,j,k-\Delta k] \right)
$$

$$
    \left( \nabla \times \textbf{A} \right)_y \approx
        \frac{1}{2 \Delta} \left( A_x[i,j,k + \Delta] - A_x[i,j,k - \Delta] - A_z[i + \Delta,j,k] + A_z[i-\Delta,j,k] \right)
$$

$$
    \left( \nabla \times \textbf{A} \right)_z \approx
        \frac{1}{2 \Delta} \left(A_y[i + \Delta,j,k] - A_y[i - \Delta,j,k] - A_x[i,j+ \Delta,k] + A_x[i,j - \Delta,k] \right)
$$

The discretization of time, with step $\delta t$, is not independent of the discretization of space, with step $\Delta$ (TODO: discuss Courant number  https://en.wikipedia.org/wiki/Courant%E2%80%93Friedrichs%E2%80%93Lewy_condition)

Finally, we arrive at the discretized update equations. To emphasize the discrete nature more obvious, and for easier porting to software later, we will separate the fields $\textbf{H}$ and $\textbf{E}$ each into three equations -- one per dimension -- and use square brackets for the $x$, $y$, $z$, and $t$ coordinate:

$$
\begin{align}
H_x[i,j,k,n+\tfrac{\delta n}{2}]
    &= H_x[i,j,k,n-\tfrac{\delta n}{2}]
    &- \frac{\delta n}{\mu [i,j,k,n]}
        \frac{1}{2 \Delta}
            \left(
                E_z[i,j+\Delta, k,n]
                - E_z[i,j-\Delta, k,n]
                - E_y[i,j,k+\Delta,n]
                + E_y[i,j,k-\Delta k,n]
            \right) \\
H_y[i,j,k,n+\tfrac{\delta n}{2}]
    &= H_y[i,j,k,n-\tfrac{\delta n}{2}]
    &- \frac{\delta n}{\mu [i,j,k,n]}
        \frac{1}{2 \Delta}
            \left(
                E_x[i,j,k + \Delta,n]
                - E_x[i,j,k - \Delta,n]
                - E_z[i + \Delta,j,k,n]
                + E_z[i-\Delta,j,k,n]
            \right) \\
H_z[i,j,k,n+\tfrac{\delta n}{2}]
    &= H_z[i,j,k,n-\tfrac{\delta n}{2}]
    &- \frac{\delta n}{\mu [i,j,k,n]}
        \frac{1}{2 \Delta}
            \left(
                E_y[i + \Delta,j,k,n]
                - E_y[i - \Delta,j,k,n]
                - E_x[i,j+ \Delta,k,n]
                + E_x[i,j - \Delta,k,n]
            \right) \\
E_x[i,j,k,n+\delta n]
    &= E_x[i,j,k,n]
    &+ \frac{\delta n}{\epsilon [i,j,k,n]}
        \frac{1}{2 \Delta}
            \left(
                H_z[i,j+\Delta, k,n+\tfrac{\delta n}{2}]
                - H_z[i,j-\Delta, k,n+\tfrac{\delta n}{2}]
                - H_y[i,j,k+\Delta,n+\tfrac{\delta n}{2}]
                + H_y[i,j,k-\Delta k,n+\tfrac{\delta n}{2}]
            \right) \\
E_y[i,j,k,n+\delta n]
    &= E_y[i,j,k,n]
    &+ \frac{\delta n}{\epsilon [i,j,k,n]}
        \frac{1}{2 \Delta}
            \left(
                H_x[i,j,k + \Delta,n+\tfrac{\delta n}{2}]
                - H_x[i,j,k - \Delta,n+\tfrac{\delta n}{2}]
                - H_z[i + \Delta,j,k,n+\tfrac{\delta n}{2}]
                + H_z[i-\Delta,j,k,n+\tfrac{\delta n}{2}]
            \right) \\
E_z[i,j,k,n+\delta n]
    &= E_z[i,j,k,n]
    &+ \frac{\delta n}{\epsilon [i,j,k,n]}
        \frac{1}{2 \Delta}
            \left(
                H_y[i + \Delta,j,k,n+\tfrac{\delta n}{2}]
                - H_y[i - \Delta,j,k,n+\tfrac{\delta n}{2}]
                - H_x[i,j+ \Delta,k,n+\tfrac{\delta n}{2}]
                + H_x[i,j - \Delta,k,n+\tfrac{\delta n}{2}]
            \right)
\end{align}
$$

