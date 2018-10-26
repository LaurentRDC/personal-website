---
title: Angels & Demons as an illustration of search algorithms.
date: 2018-10-18
updated: 2018-10-25
summary: This post is about what the story of Angel & Demons can teach us about linear vs. binary search, by optimizing how much time is spent looking for a bomb in Vatican City.
---

I was watching the movie [Angels & Demons](https://en.wikipedia.org/wiki/Angels_%26_Demons_(film)) recently, and it got me thinking about binary search trees.

## Quick summary of Angels & Demons

In the movie, an explosive device (based on antimatter!) is placed somewhere inside Vatican City. The bomb is set to explode around midnight, leaving about 4-5h of search time. However, to taunt our protagonists, the bomb maker has placed a wireless camera continously showing the bomb:

![Canister of antimatter in the movie Angels & Demons](/images/angels_demons/antimatter.png)

Notice how the bomb is artificially lit. This leads the security forces to try and locate the bomb by shutting off the power in various zones of Vatican City. If they see the articificial lighting being shut off on camera, then they'll have zoned in on the bomb.

## How to find the bomb quickly

It would be a boring movie if the security forces found the bomb this way. However, this got me thinking : how should they proceed in order to zone in on the bomb as fast as possible? For this exercise, let's assume th at the Vatican City power grid is divided into 100 zones.

The simplest way to locate the bomb is as follows:

1. Shut off power to zone 1;

2. Look at the bomb camera to see if bomb is in zone 1;

3. If bomb is not in zone 1, power back zone 1 and move on to zone 2.

4. Repeat for zones 2, 3, etc. until bomb is found.

I claim that this way of searching is not the fastest! There is a much faster way to find the bomb. Before reading further, try to think of a better way to search.

The better way to search for the bomb is as follows:

1. Shut off power to zones 1 through 50, effectively shutting off the power in half the city.

2. If the lighting on the bomb is still active, then the bomb is somewhere in zones 51-100. Otherwise, the bomb is in zones 1-50.

3. Repeat the process in the appropriate zones, halving the total area of power shutdown.

It turns out that this second process of halving the search area at each step, would be more than __10x__ faster than the simple process described above!

## Optimizing search is a general problem!

It turns out that this situation from Angels & Demons is a general problem in computer science. The problem is:

_What is the fastest way to search for a specific object in an __ordered__ group of objects?_

Note that the existence of ordering is important, i.e. can elements be compared. An example of this problem is finding the Vatican City zone containing a bomb. In this case, the zones are ordered geographically. Another example is checking whether someone's name is on a guest list. In this case, the names are ordered alphabetically.

The simplest way to search, corresponding to searching for the bomb zone-by-zone, is called __linear search__. The faster way to search, corresponding to halving the search zone at each step, is called __binary search__. How much faster is binary search? Let's look at the number of steps for the worst case:

```{plot_target=generated/bin_search_worst_case.jpg plot_alt="Number of search steps required to find the Angels & Demons bomb as a function of (greatly exaggerated) number of zones, in the worst case. For searches in very large spaces, binary search is much faster! Note that the ordinate axis is in logarithmic scale."}
import matplotlib.pyplot as plt
import numpy as np

search_size = np.arange(50, 10000, 50)
# Linear search has linear worst-case complexity
linear_complexity = search_size
# Binary search has logarithmic worst-case complexity
log_complexity = np.round(np.log2(search_size))

plt.figure()
plt.semilogy(search_size, linear_complexity, '+r', label = "Linear search")
plt.semilogy(search_size, log_complexity, 'ob', label = "Binary search")
plt.xlabel("Number of zones")
plt.ylabel("Number of search steps")
plt.legend(loc=7)
```

Those of you with a knowledge of [computational complexity](https://en.wikipedia.org/wiki/Computational_complexity) will recognize the shapes of those

The worst case search scenario is much faster for binary search. In our Vatican City example, where there are 100 search zones, binary search will end in about 7 steps at worst, vs. 100 steps in the linear search worst case. That's a huge difference! 

In examples where the search space is much larger (e.g. searching for a name in a database), binary search gets even better.

## Worst case vs. average case
Of course, the best-case scenario for linear search is __1 step__. What's the average case? Going back to the bomb example, let's imagine that the bomb is randomly placed. Then, the probability it is located in a zone is always $1/100$.
$$
\begin{align}
    \text{Average number of search steps} &= \sum_i \frac{\text{Number of steps if bomb in zone i}}{\text{Probability that bomb is in zone i}}\\
    &= \sum_{i} \frac{i}{100}\\
    &= \frac{1}{100} \sum_{i=1}^{100} i \\
    &\approx 50 \\
\end{align}
$$

In general, the average number of steps required to find the bomb among $N$ zones, using linear search, is:
$$
\begin{align}
    \text{Average number of search steps} &= \sum_{i=1}^{N} \frac{i}{N} \\
    &= \frac{1}{N} \sum_{i=1}^{N} i \\
    &= \frac{1}{N} \frac{N \cdot(N+1)}{2} \\
    &\approx N/2 \\
\end{align}
$$

What about for binary search? Well, we need to divive the search zone in halves until the search zone has the same size as one zone. This can be expressed as the following equation:
$$
\begin{align}
    \text{Size of search zone after } i \text{ halvings} &= 1 \\
    \frac{N}{(2^i)} &= 1
\end{align}
$$

Rearranging terms:
$$
N = 2^i \rightarrow i = \text{Number of search steps} = \log_2(N)
$$

Therefore, on average, linear search is __much worse__ than binary search. If you ever have to search for something quickly, consider using a variant of binary search!

## Aside: in the programming world

If you don't know/care about programming, you can stop reading now.

Have you ever wondered why `set` exists in Python? This is precisely because of linear vs binary search. When creating a Python `set`, objects are __hashed__ (i.e. given a unique ID number)[^1], and the objects are stored such that their ID numbers are ordered. Therefore, to find an arbitrary object in a `set` requires a binary search ammongst ID numbers. On the other hand, search through a Python `list` is a linear search through all the terms.

Bottom line : For general purpose, feel free to use a `list`. If you want to search through something, consider using a `set`!

Fun fact : due to the way Python `dict` are implemented, finding dictionary keys is also a binary search.

Then there's Haskell. In Haskell, if you often search through a structure, consider using the structures provided by the [`containers`](https://hackage.haskell.org/package/containers) or [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers)[^2] modules. The choice between then depends on your use case; take a look at [haskell-perf](https://github.com/haskell-perf/sets) organization for a look at how these modules compare.

[^1]: There is so much more to hashing than that! I'll probably write about hashing in more details soon.

[^2]: In this case, ordered container means that the order of elements placed in the containers is predictable, like a list. An unordered container is then more like a bag: there is no notion of numerical ordering.