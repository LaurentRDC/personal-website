---
title: On the significance of chirped pulse amplification and the Nobel Prize in Physics 2018
date: 2018-10-05
---

This week, the Nobel Prize in Physics 2018 was announced : half of it goes to Donna Strickland and Gérard Mourou for the invention of a _"(...) method of generating high-intensity, ultra-short optical pulses"_.

This method, called chirped-pulse amplification (CPA), dates back to 1985 [^1]. At the time, it could produce pulses of 2 picoseconds duration (2 picoseconds = $2^{-12}$ seconds = 0.000 000 000 002 seconds), with each pulse carrying about 1 milliJoules (mJ).

Practically, in 2018, commercial laser systems can produce pulses of ~30 femtoseconds ($30^{-15}$ seconds), with each pulse carrying > 3 mJ. You can buy one for yourself, [for example here](https://www.spectra-physics.com/products/ultrafast-lasers/), for a few hundred thousands $CA. That might sound like a lot to you, but not in terms of cutting-edge research.

These laser systems are becoming ubiquitous. [I use one everyday](http://www.physics.mcgill.ca/siwicklab). So do many of my colleagues in the Physics department ([here](http://www.physics.mcgill.ca/~cooke) and [here](http://physics.mcgill.ca/~grutter)) and Chemistry department ([this guy has two](http://kambhampati-group.mcgill.ca/)) at McGill University alone. The technology is transformative, and hence the Nobel Prize is well-deserved.

In the media, chirped pulse amplification is touted as a way of creating petawatts-laser system. This is misleading, and misses the point on what chirped pulse amplification has helped achieve in the past 30 years.

### What does it mean to have a petawatt lasers

Let's first talk about what watts are.

The microwave in your home is probably rated to use 1200 watts of power. What does this mean? Watts are defined as:
$$
    1 \text{ watt} = \frac{\text{energy in Joules}}{\text{time in seconds}}
$$
Therefore, a 1200 watts microwave consumes 1200 Joules/second. That's not super helpful if you have never heard of Joules; in terms of kilocalories (the units of energy used in nutritional labels), a 1200 Watts microwave consumes ~0.3 kcal/seconds. All this to say that a 1200 watts microwave running for three minutes consumes about 50 kcal, which is not that much. That's the equivalent of walking for a few minutes.

At the other extreme end, the average US energy consumption is about 3 petawatts (3 trillion watts). It would be absolutely crazy for a laser system to consume the same amount of energy as the entire US. Actually, CPA isn't used to create laser systems with an _average_ power of a petawatt, but rather with a __peak power__ of a petawatt.

Take our laser system, for example (Spectra-Physics Spitfire Pro 3W). The _average_ power of the laser is about 3 watts, which sounds pretty low for compared to our microwave example earlier. However, because the pulses are so short, the __peak power__ of our laser is:
$$
    \begin{align}
    \text{Peak power in watts}  &= \frac{\text{Energy of a pulse}}{\text{Duration of a pulse}} \\\\
                                &= \frac{0.003 \text{ Joules}}{30 \text{ femtoseconds}} \\\\
                                &= \frac{0.003 \text{ Joules}}{0.000000000000030 \text{ seconds}}\\\\
                                &= 0.1 \text{ petawatts}
    \end{align}
$$

Therefore, "petawatt" lasers are called "petawatt" because that's their __peak power__. The energy in each pulse is rather low (0.003 Joules is small by human standard), but because this energy is packed into a very short pulse (0.00000000000003 seconds), the energy per time during a pulse is enormous.

__This is the claim-to-fame of chirped pulse amplification__: how to make super short pulses while increasing the energy in each pulse from super-low to not-that-low. Making super short pulses was possible before, but at the expense of pulse energy.

### What can you do with a petawatt laser?

I'm going to speak from my personal experience here.

Having very short laser pulses with enough energy was the start of the field of ultrafast material science. With chirped pulse amplification, we can now dump a reasonable amount of energy in a very short time-frame. We can use other ultrafast tools to then measure the reaction of materials after all this energy has been dumped. If chirped pulse amplification didn't exist, we would barely tickle materials at all, and thus couldn't make measurements in these experiments. This is definitely a topic for another time.

We can also generate ultra-short pulses of other things, using ultra-short laser pulses. In my research group, we generate ultra-short pulses of electrons using the [photoelectric effect](https://en.wikipedia.org/wiki/Photoelectric_effect) (another Nobel Prize, Physics 1921!) and use these super-short electron pulse [for great good](http://www.physics.mcgill.ca/siwicklab/research.html). Again, a topic for another time.

Other uses for super-short, energetic laser pulses that new light-matter interactions appear. For high-enough peak power, the interaction between laser pulses and matter can be weird. This is not new - regular ol' continuous lasers can also be used to measure nonlinear effects - but we can now reach new regimes of nonlinearity with ultrahigh peak power. That is not a topic for another time.

### Closing remarks

While you may never have heard of Donna Strickland and Gérard Mourou before, their work has profoundly impacted current-day research. I could not do what I do without chirped pulse amplification.

On a different note, this Physics Nobel Prize also marks an important event: Donna Strickland was a graduate student in 1985. She is being rewarded for work she did under the supervision of her thesis advisor Gérard Mourou. I hope this marks the end of graduate students working in the shadow of their supervisors. I present [Jocelyn Bell Burnell](https://en.wikipedia.org/wiki/Jocelyn_Bell_Burnell) as an infuriating example.

[^1]: D. Strickland and G. Mourou, _Compression of amplified chirped optical pulses_ (1985) . Optics Communications __56__, pp. 219-221. DOI: [10.1016/0030-4018(85)90120-8](https://doi.org/10.1016%2F0030-4018%2885%2990120-8)
 