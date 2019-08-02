---
title: Simulating a PID controller with Python
date: 2019-08-02
summary: We are testing the implementation of a new proportional-integral-derivative (PID) controller in our lab. How can we simulate its effects first?
withtoc: true
---

## Our master clock

I work in an [ultrafast electron scattering](http://www.physics.mcgill.ca/siwicklab) research group, where we combine ultrafast laser systems with prototype (read: homemade) electron microscopes. Ultrafast lasers are strange beasts. They're not quite as easy-to-use as continuous wave (CW) lasers. 

I'll spare you the details here, but the output frequency of a particular part of the laser -- the oscillator, or master clock -- drifts a little bit over time. It might drift by 10 Hz per hour, nothing crazy, but I need this to be 0 Hz forever. The way to model this behavior is similar to random walks. Each step in the walk is a fluctuation of the clock. The final location of the walk -- the clock frequency -- is a cumulative sum of steps, or frequency jolts. In Python, we can represent it for 1800s (1h) as follows:

```python
import numpy as np

jolts = np.random.normal(loc = 0, scale = 0.1, size=(1800,))
drift = np.cumsum(jolts)
```
where `np.cumsum` is the [cumulative sum function](https://docs.scipy.org/doc/numpy/reference/generated/numpy.cumsum.html), not to be confused with the usual `np.sum`. Here, we are assuming that every second, there is a 66% change that the frequency will change by 0.1Hz, i.e. jolts are picked from a Normal distribution with standard deviation of 0.1 (`scale=0.1`). 

Over a half hour (1800 seconds) the output frequency might look like this:
```{.pyplot caption="Example of the output frequency drift of our master clock. The initial setpoint (zero-drift) is represented by a dashed line."}

time = np.arange(0, 1800)

# The drift is the cumulative effect of random steps, like a random walk
# We round to nearest integer because that is the resolution of our frequency measurement
drift = np.cumsum(
        np.random.normal(loc=0, scale=0.1, size=time.shape)
    )

plt.figure(figsize=(6,3))
plt.axhline(y=0, linestyle='--', color='k')
plt.plot(time, drift, '.k')
plt.xlabel('Time [s]')
plt.ylabel('Drift [Hz]')
```

Our master clock (an older [Spectra-Physics Tsunami](https://www.spectra-physics.com/products/ultrafast-lasers/tsunami)) comes with the ability to control the frequency. However, to save cost (~ 30 000 $US), we didn't buy the control module, so we need to do this ourselves. The standard mechanism for control systems of this sort is a [proportional–integral–derivative](https://en.wikipedia.org/wiki/PID_controller) (PID) controller. 

## PID controllers

I don't understand PID controllers fully, but let me attempt to give some insight into how they work. PID controllers apply a correction signal $u(t)$ based on the input signal $y(t)$ (in the case, clock frequency) so that the corrected signal is as close as possible to a set-point. Here's a conceptual block diagram:

![Block diagram of a PID controller. Here, the setpoint $r(t)$ might change in time. The correction signal is $u(t)$. The corrected signal is $y(t)$. The deviation of the signal from out setpoint is the error signal, $e(t)$. Credit to Arturo Urquizo.](/images/pid/PID.svg)

A PID controller is parametrized by three constants: the proportional factor $K_p$, the integral factor $K_i$, and the derivative factor $K_d$. These factors encode three behaviors:

* The magnitude of the response of the controller to an error is set by the proportional factor $K_p$;
* The importance attributed to recent past error values is set by the integral factor $K_i$;
* The important of the estimation of future trend is set by the derivative factor $K_d$. 

We will explore the effects of $K_p$, $K_i$, and $K_d$ after implementing a complete system.

### Mathematical description

What should be the correction signal $u(t)$? We can state it mathematically as follows:
$$
u(t) = K_p e(t) + K_i \int_0^t e(\tau) d\tau + K_d \frac{de(t)}{dt}
$$
where
$$
e(t) = y(t) - r(t)
$$
$e(t)$ is the deviation of the system from the set-point $r(t)$. 

## Modeling with Python

I can think of two ways to model the behavior of the our clock + PID controller.

1. Numerically evaluate $u(t)$ based on a simulated $y(t)$ by solving the above equation.
2. Implement an object-oriented interface and iterate

You might think that approach 2 is redundant, but it is much closer to a real-time software implementation where a computer can act as a PID.

### Method 1: Numerical evaluation

I find it easiest to think about numerical evaluation by imagining that the universe is perfectly deterministic. Therefore, the slow jitter $j(t)$ in the clock frequency $y(t) = r + j(t)$ can be simulated in advance. That's how we start:
```python
import numpy as np

# We seed the random number generator so that the random signal
# does not change everytime we run this simulation. This is optional.
np.random.seed(2019)

# We simulate for a half hour, which is 1800 seconds
times = np.arange(0, 1800)
error_signal = np.cumsum(
        np.random.normal(loc = 0, scale = 0.1, size=(1800,))
    )
```

#### Proportional response

We'll have a function that evaluates the correction signal $u(t)$ only with a proportional constant:
```python
def pid_correction(times, proportional, integral=0, derivative=0):
    """ Evaluate the correction signal """
    return -1 * (proportional * error_signal)
```
Let's try it in action:
```{.pyplot caption="Example of a proportional feedback system, with $K_p=0.8$, which is not optimal."}
np.random.seed(2019)

# We simulate for a half hour, which is 1800 seconds
times = np.arange(0, 1800)
error_signal = np.cumsum(
        np.random.normal(loc = 0.01, scale = 0.1, size=(1800,))
    )

def pid_correction(times, proportional, integral=0, derivative=0):
    """ Evaluate the correction signal """
    return -1 * (proportional * error_signal)

correction = pid_correction(times=times, proportional=0.8)
corrected = error_signal + correction

fig, (ax, residuals) = plt.subplots(nrows=2, ncols=1, sharex=True, figsize=(8,4))

ax.axhline(y=0, linestyle=':', color='k')
h1, = ax.plot(times, error_signal,'.r', label='Error')
h2, = ax.plot(times, correction, '.k', label='Applied correction')

residuals.axhline(y=0, linestyle=':', color='k')
h3, = residuals.plot(times, corrected, '.g', label='Output')

ax.legend(handles=[h1,h2,h3], loc='center', ncol=3, bbox_to_anchor=(0.5, 1.1))

residuals.set_xlabel('Time [s]')
residuals.set_ylabel('Residuals [Hz]')
ax.set_ylabel('Signal [Hz]')

plt.subplots_adjust(hspace=0)
```

The plot above looks pretty good. It's obvious that if I had chosen $K_p=1$, the correction would have been perfect. But in real life, feedback systems are not linear. Applying a proportional correction is too easy. In our master clock, the device that adjusts the frequency only works for $|u(t)| > 1$. The correction looks like this:
$$
\left\{
\begin{array}{ll}
      u(t) + 1 & u(t) < 1 \\
      0 & -1 \geq u(t) \leq 1 \\
      u(t) - 1 & u(t) > 1 \\
\end{array} 
\right.
$$
Visually:
```{.pyplot caption="Feedback response function. One would expect that the response would be proportional, but the device has an 'activation' threshold of $\pm 1$ V"}
voltage = np.linspace(-3, 3, 512)
response = np.zeros_like(voltage)
response[voltage < -1] = voltage[voltage < -1] + 1
response[voltage >  1] = voltage[voltage > 1] - 1

plt.axvline(x=-1, linestyle=':', color='k')
plt.axvline(x=1,  linestyle=':', color='k')
plt.axhline(y=0,  linestyle=':', color='k')
plt.plot(voltage, response, '-b', label='Reality')
plt.plot(voltage, voltage, '--k', label='Expected')
plt.xlabel('Input voltage [V]')
plt.ylabel('Response [Hz]')
plt.legend(loc='center', ncol=2, bbox_to_anchor=(0.5, 1.05))
```

#### Proportional-integral response

Let's implement the integral term $K_i \int_0^t e(\tau)d\tau$. The cumulative integral is best approximated by the [`scipy.integrate.cumtrapz`](https://docs.scipy.org/doc/scipy/reference/generated/scipy.integrate.cumtrapz.html#scipy-integrate-cumtrapz) function:
```python
from scipy.integrate import cumtrapz

def pid_correction(times, proportional, integral, derivative=0):
    """ Evaluate the correction signal """
    proportional_response = proportional * error_signal
    integral_response     = integral     * cumtrapz(y=error_signal, x=times, initial=0)
    derivative_response   = 0

    # Collect the contribution from all contants
    instantaneous = -1 * (proportional_response + 
                          integral_response + 
                          derivative_response
                         )

    # Simulation of the effect of a one-step delay
    response = np.zeros_like(times, dtype=np.float)
    response[1::] = instantaneous[0:-1]
    return response
```

```{.pyplot caption="Example of a proportional-integral feedback system, with $K_p=0.3$ and $K_i=0.01$, taking into account the effect of a 1s delay (one time-step)."}
from scipy.integrate import cumtrapz
np.random.seed(2019)

# We simulate for a half hour, which is 1800 seconds
times = np.arange(0, 1800)
error_signal = np.cumsum(
        np.random.normal(loc = 0.01, scale = 0.1, size=(1800,))
    )

def pid_correction(times, proportional, integral, derivative=0):
    """ Evaluate the correction signal """
    proportional_response = proportional * error_signal
    integral_response     = integral     * cumtrapz(y=error_signal, x=times, initial=0)
    derivative_response   = 0

    # Collect the contribution from all contants
    instantaneous = -1 * (proportional_response + 
                          integral_response + 
                          derivative_response
                         )

    # Simulation of the effect of a one-step delay
    response = np.zeros_like(times, dtype=np.float)
    response[1::] = instantaneous[0:-1]
    return response

correction = pid_correction(times=times, proportional=1, integral=0)
corrected = error_signal + correction

fig, (ax, residuals) = plt.subplots(nrows=2, ncols=1, sharex=True, figsize=(8,4))

ax.axhline(y=0, linestyle=':', color='k')
h1, = ax.plot(times, error_signal,'.r', label='Error')
h2, = ax.plot(times, correction, '.k', label='Applied correction')

residuals.axhline(y=0, linestyle=':', color='k')
h3, = residuals.plot(times, corrected, '.g', label='Output')

ax.legend(handles=[h1,h2,h3], loc='center', ncol=3, bbox_to_anchor=(0.5, 1.1))

residuals.set_xlabel('Time [s]')
residuals.set_ylabel('Residuals [Hz]')
ax.set_ylabel('Signal [Hz]')

plt.subplots_adjust(hspace=0)
```

### Method 2: Real-time system