---
title: Genesis
date: 2018-08-01
summary: This is the first post of my blog, where I test new features.
---

This is the first post of my blog, where I can test new features.

---

### Math

Math display works:

$$ 
\begin{align}
    \nabla \cdot \textbf{E} &= \frac{\rho}{\epsilon_0} \\
    \nabla \cdot \textbf{B} &= 0 \\
    \nabla \times \textbf{E} &= -\frac{\partial \textbf{B}}{\partial t} \\
    \nabla \times \textbf{B} &= \mu_0 \left( \textbf{J} + \epsilon_0 \frac{\partial \textbf{E}}{\partial t} \right) \\
    \frac{\partial \rho}{\partial t} + \nabla \cdot \textbf{J} &= 0
\end{align}
$$  


---

### Code

Code blocks also work, including syntax highlighting:

```haskell
-- Haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

```python
# Python
def fib(n):
    i, j = 0, 1
    for _ in range(n):
        yield i
        i, j = j, i + j
```

---

### Images

![Caption: this is me](/images/Laurent.jpg)

---

### Quotes

>  The number of bins (of size 1) is one larger than the largest value in x. If minlength is specified, there will be at least this number of bins in the output array (though it will be longer if necessary, depending on the contents of x).

- _NumPy documentation for bincount_

### Update 2018-09-30 : Python plots

Using this space to test the use of my first published Haskell package, [`pandoc-pyplot`](http://hackage.haskell.org/package/pandoc-pyplot)

You can click on the image to see the source Python code used to generate the figure.

```{plot_target=generated/genesis.jpg}
import matplotlib.pyplot as plt

plt.figure()
plt.plot([1,2,3,4,5], [1,2,3,4,5])
plt.title('Test plot')
```