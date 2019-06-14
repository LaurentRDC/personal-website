---
name: pandoc-pyplot
description: A Pandoc filter to generate Matplotlib figures directly in documents
repository: https://github.com/LaurentRDC/pandoc-pyplot
---

<a href="http://hackage.haskell.org/package/pandoc-pyplot" target="_blank">
    <img src="https://img.shields.io/hackage/v/pandoc-pyplot.svg">
</a>
<a href="http://stackage.org/nightly/package/pandoc-pyplot" target="_blank">
    <img src="http://stackage.org/package/pandoc-pyplot/badge/nightly">
</a>

`pandoc-pyplot` is a Pandoc filter in the spirit of sphinx's [`plot_directive`](https://matplotlib.org/devel/plot_directive.html). Write Python code to make a Matplotlib figure in your Markdown , and `pandoc-pyplot` will generate the figure for you and embed it in the document. This way, your documents and figures are always up-to-date with each other.

For example, the following code block:

```markdown

    ```{.pyplot caption="This example was taken from Matplotlib's [example gallery](https://matplotlib.org/examples/pie_and_polar_charts/polar_scatter_demo.html)"}
    import numpy as np
    import matplotlib.pyplot as plt

    N = 150
    r = 2 * np.random.rand(N)
    theta = 2 * np.pi * np.random.rand(N)
    area = 200 * r**2
    colors = theta

    ax = plt.subplot(111, projection='polar')
    c = ax.scatter(theta, r, c=colors, s=area, cmap='hsv', alpha=0.75)
    ```

```

will be directly rendered in the document:

```{.pyplot directory="images/pandoc-pyplot-gallery/" caption="This example was taken from Matplotlib's [example gallery](https://matplotlib.org/examples/pie_and_polar_charts/polar_scatter_demo.html)"}
N = 150
r = 2 * np.random.rand(N)
theta = 2 * np.pi * np.random.rand(N)
area = 200 * r**2
colors = theta

ax = plt.subplot(111, projection='polar')
c = ax.scatter(theta, r, c=colors, s=area, cmap='hsv', alpha=0.75)
```