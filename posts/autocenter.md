---
title: Harnessing symmetry to find the center of a diffraction pattern
date: 2021-01-23
summary: Diffraction patterns are very symmetric; how hard can it be to find its center?
---

Ultrafast electron diffraction involves the analysis of *diffraction patterns*. Here is an example diffraction pattern for a thin (<100nm) flake of graphite[^1]:

```{.python .matplotlib caption="Diffraction pattern of graphite"}
from skued import diffread
from pathlib import Path

im = diffread(Path("images") / "autocenter" / "graphite.tif")

fig, ax = plt.subplots(1, 1)
ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
ax.axis('off')
plt.tight_layout()
```

A diffraction pattern is effectively the intensity of the Fourier transform. Given that crystals like graphite are well-ordered, the diffraction peaks (i.e. Fourier components) are very large. You can see that the diffraction pattern is six-fold symmetric; that's because the atoms in graphite arrange themselves in a honeycomb pattern, which is also six-fold symmetric. In these experiments, the fundamental Fourier component is so strong that we need to block it. That's what that black *beam-block* is about.

There are crystals that are not as well-ordered as graphite. Think of a powder made of many small crystallites, each being about 50nm x 50nm x 50nm. Diffraction electrons through a sample like that results in a kind of average of all possible diffraction patterns. Here's an example with polycrystalline Chromium:

```{.python .matplotlib caption="Diffraction pattern of polycrystalline Chromium"}
from skued import diffread
from pathlib import Path

im = diffread(Path("images") / "mnxc" / "Cr_1.tif")

fig, ax = plt.subplots(1, 1)
ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
ax.axis('off')
plt.tight_layout()
```

Each ring in the above pattern pattern corresponds to a Fourier component. Notice again how symmetric the pattern is; the material itself is symmetric enough that the fundamental Fourier component needs to be blocked.

For my work on [iris-ued](https://github.com/LaurentRDC/iris-ued), a data analysis package for ultrafast electron scattering, I needed to find a reliable, automatic way to get the center of such diffraction patterns to get rid of the manual work required now. So let's see how!

## First try: center of mass

A first naive attempt might start with the *center-of-mass*, i.e. the average of pixel positions weighted by their intensity. Since intensity is symmetric about the center, the *center-of-mass* should coincide with the actual physical center of the image.

Good news, [scipy's `ndimage` module](https://docs.scipy.org/doc/scipy/reference/ndimage.html#multidimensional-image-processing-scipy-ndimage) exports such a function: [`center_of_mass`](https://docs.scipy.org/doc/scipy/reference/generated/scipy.ndimage.center_of_mass.html#scipy.ndimage.center_of_mass). Let's try it:

```{.python .matplotlib caption="Demonstration of using `scipy.ndimage.center_of_mass` to find the center of diffraction patterns."}
from skued import diffread
from pathlib import Path
from scipy.ndimage import center_of_mass

im1 = diffread(Path("images") / "autocenter" / "graphite.tif")
im2 = diffread(Path("images") / "mnxc" / "Cr_1.tif")

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(6,3))

for im, ax in zip([im1, im2], [ax1, ax2]):

    r, c = center_of_mass(im)
    ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
    ax.scatter(c, r, color='r')
    ax.axis('off')

plt.tight_layout()
```

Not bad! Especially in the first image, really not a bad first try. But I'm looking for something *pixel-perfect*. Intuitively, the beam-block in each image should mess with the calculation of the center of mass. Let's define the following areas that we would like to ignore:

```{.python .matplotlib caption="Areas that are bright are defined as being masked"}
from skued import diffread
from pathlib import Path

im1 = diffread(Path("images") / "autocenter" / "graphite.tif")
mask1 = diffread(Path("images") / "autocenter" / "graphite_mask.tif").astype(bool)

im2 = diffread(Path("images") / "mnxc" / "Cr_1.tif")
mask2 = np.ones_like(im2, dtype=bool)
mask2[0:1250, 950:1250] = False

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(6,3))

for ax, im, mask in zip([ax1, ax2], [im1, im2], [mask1, mask2]):

    im[np.logical_not(mask)] = 200
    ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
    ax.axis('off')

plt.tight_layout()
```

Masks are generally defined as boolean arrays with True (or 1) where pixels are valid, and False (or 0) where pixels are invalid. Therefore, we should ignore the weight of masked pixels. `scipy.ndimage.center_of_mass` does not support this feature; we need an extension of `center_of_mass`:

```python
def center_of_mass_masked(im, mask):
    rr, cc = np.indices(im.shape)
    weights = im * mask.astype(im.dtype)

    r = np.average(rr, weights=weights)
    c = np.average(cc, weights=weights)
    return r, c
```

This is effectively an average of the row and column coordinates (`rr` and `cc`) weighted by the image intensity. The trick here is that `mask.astype(im.dtype)` is 0 where pixels are "invalid"; therefore they don't count in the average! Let's look at the result:

```{.python .matplotlib caption="Demonstration of using `center_of_mass_masked` (see above) to find the center of diffraction patterns."}
from skued import diffread
from pathlib import Path

def center_of_mass_masked(im, mask):
    rr, cc = np.indices(im.shape)
    weights = im * mask.astype(im.dtype)

    r = np.average(rr, weights=weights)
    c = np.average(cc, weights=weights)
    return r, c

im1 = diffread(Path("images") / "autocenter" / "graphite.tif")
mask1 = diffread(Path("images") / "autocenter" / "graphite_mask.tif").astype(bool)

im2 = diffread(Path("images") / "mnxc" / "Cr_1.tif")
mask2 = np.ones_like(im2, dtype=bool)
mask2[0:1250, 950:1250] = False

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(6,3))

for ax, im, mask in zip([ax1, ax2], [im1, im2], [mask1, mask2]):

    r, c = center_of_mass_masked(im, mask)
    ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
    ax.scatter(c, r, color='r')
    ax.axis('off')

plt.tight_layout()
```

I'm not sure if it's looking better, honestly. But at least we have an approximate center! That's a good starting point that feeds in to the next step.

## Friedel pairs and radial inversion symmetry

In his thesis[^2], which is now also [a book](https://www.springer.com/us/book/9783030548506?utm_campaign=3_pier05_buy_print&utm_content=en_08082017&utm_medium=referral&utm_source=google_books#otherversion=9783030548513), Nelson Liu describes how he does it:

>  A rough estimate of its position is obtained by calculating the ‘centre of intensity’ or intensity-weighted arithmetic mean of the position of > 100 random points uniformly distributed over the masked image; this is used to match diffraction spots into Friedel pairs amongst those found earlier. By averaging the midpoint of the lines connecting these pairs of points, a more accurate position of the centre is obtained.

Friedel pairs are peaks related by inversion through the center of the diffraction pattern. The existence of these pairs is guaranteed by crystal symmetry. For polycrystalline patterns, Friedel pairs are averaged into rings; rings are always inversion-symmetric about their centers. Here's an example of two Friedel pairs:

```{.python .matplotlib caption="Example of two Friedel pairs: white circles form pair 1, while red circles form pair 2."}
from skued import diffread, autocenter
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from skimage.transform import rotate

im = diffread(Path("images") / "autocenter" / "graphite.tif")
mask = diffread(Path("images") / "autocenter" / "graphite_mask.tif")
r, c = autocenter(im=im, mask=mask)
im = rotate(im, center=(c, r), angle=8, mode="reflect", preserve_range=True)

fig, ax = plt.subplots(1, 1, figsize=(4, 4))

ax.imshow(im, vmin=0, vmax=200, cmap="inferno")

pair1 = [
    mpatches.Circle(xy=(c - 281, r), radius=35, ec="w", fc="none"),
    mpatches.Circle(xy=(c + 281, r), radius=35, ec="w", fc="none"),
]

pair2 = [
    mpatches.Circle(xy=(c - 138, r + 246), radius=35, ec="r", fc="none"),
    mpatches.Circle(xy=(c + 138, r - 246), radius=35, ec="r", fc="none"),
]

for p in pair1 + pair2:
    ax.add_patch(p)
ax.axis("off")

plt.tight_layout()
```

The algorithm by Liu was meant for single-crystal diffraction patterns with well-defined peaks, and not so much for rings. However, we can distill Liu's idea into a new, more general approach. If the approximate center coincides with the actual center of the image, then the image should be invariant under radial-inversion with respect to the approximate center. Said another way: if the image $I$ is defined on polar coordinates $(r, \theta)$, then the center maximizes correlation between $I(r, \theta)$ and $I(-r, \theta)$. Thankfully, computing the masked correlation between images [is something I've worked on before](/posts/mnxc.html)!

Let's look at what radial inversion looks like. There are ways to do it with interpolation, e.g. [scikit-image's `warp` function](https://scikit-image.org/docs/stable/api/skimage.transform.html#warp). However, in my testing, this is incredibly slow compared to what I will show you. A faster approach is to consider that if the image was centered on the array, then radial inversion is really flipping the direction of the array axes; that is, if the image array `I` has size (128, 128), and the center is at (64, 64), the radial inverse of `I` is `I[::-1, ::-1]` (numpy) / `flip(flip(I, 1), 2)` (MATLAB) / `I[end:-1:1,end:-1:1]` (Julia). Another important note is that if the approximate center of the image is far from the center of the array, the overlap between the image and its radial inverse is limited. Consider this:

```{.python .matplotlib}
import numpy as np
import matplotlib.pyplot as plt
from skued import diffread
from pathlib import Path
from math import floor

def center_of_mass_masked(im, mask):
    rr, cc = np.indices(im.shape)
    weights = im * mask.astype(im.dtype)

    r = np.average(rr, weights=weights)
    c = np.average(cc, weights=weights)
    return int(r), int(c)

im1 = diffread(Path("images") / "autocenter" / "graphite.tif")
mask1 = diffread(Path("images") / "autocenter" / "graphite_mask.tif").astype(bool)

im2 = diffread(Path("images") / "mnxc" / "Cr_1.tif")
mask2 = np.ones_like(im2, dtype=bool)
mask2[0:1250, 950:1250] = False

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(6,3))

for ax, im, mask in zip([ax1, ax2], [im1, im2], [mask1, mask2]):

    placeholder = np.full_like(im, fill_value=im.max())

    r, c = center_of_mass_masked(im, mask)
    side_length = floor(min([r, abs(r - im.shape[0]), c, abs(c - im.shape[1])]))
    rs = slice(r - side_length, r + side_length)
    cs = slice(c - side_length, c + side_length)
    placeholder[rs, cs] = im[rs, cs]

    ax.imshow(placeholder, vmin=0, vmax=200, cmap='inferno')
    ax.axis('off')
```

If we cropped out the bright areas around the frame, then the approximate center found would coincide with the center of the array; then, radial inversion is very fast.

```{.python .matplotlib caption="Demonstration of what parts of the image to crop so that the image center coincides with the center of the array."}
import numpy as np
import matplotlib.pyplot as plt
from skued import diffread
from pathlib import Path
from math import floor

def center_of_mass_masked(im, mask):
    rr, cc = np.indices(im.shape)
    weights = im * mask.astype(im.dtype)

    r = np.average(rr, weights=weights)
    c = np.average(cc, weights=weights)
    return int(r), int(c)

im1 = diffread(Path("images") / "autocenter" / "graphite.tif")
mask1 = diffread(Path("images") / "autocenter" / "graphite_mask.tif").astype(bool)

im2 = diffread(Path("images") / "mnxc" / "Cr_1.tif")
mask2 = np.ones_like(im2, dtype=bool)
mask2[0:1250, 950:1250] = False

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(6,6))

for (ax, ax_r), im, mask in zip([(ax1, ax3), (ax2, ax4)], [im1, im2], [mask1, mask2]):

    r, c = center_of_mass_masked(im, mask)
    side_length = floor(min([r, abs(r - im.shape[0]), c, abs(c - im.shape[1])]))
    rs = slice(r - side_length, r + side_length)
    cs = slice(c - side_length, c + side_length)
    im = im[rs, cs]

    ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
    ax_r.imshow(im[::-1, ::-1], vmin=0, vmax=200, cmap='inferno')
    ax.axis('off')
    ax_r.axis('off')

plt.tight_layout()
```

Now, especially for the right column of images, it's pretty clear that the approximate center wasn't perfect. The *correction* to the approximate center is can be calculated with the masked normalized cross-correlation[^3] [^4]:

```{.python .matplotlib caption="**Top left**: diffraction pattern. **Top right**: radially-inverted diffraction pattern about an approximate center. **Bottom left**: masked normalized cross-correlation between the two diffraction patterns. **Bottom right**: 2x zoom on the cross-correlation shows the translation mismatch between the diffraction patterns."}
import numpy as np
import matplotlib.pyplot as plt
from skued import diffread
from pathlib import Path
from math import floor
from skimage.registration import phase_cross_correlation
from skimage.registration._masked_phase_cross_correlation import cross_correlate_masked


def center_of_mass_masked(im, mask):
    rr, cc = np.indices(im.shape)
    weights = im * mask.astype(im.dtype)

    r = np.average(rr, weights=weights)
    c = np.average(cc, weights=weights)
    return int(r), int(c)


def correlate(arr1, arr2, m1, m2):
    return cross_correlate_masked(arr1=arr1, arr2=arr2, m1=m1, m2=m2, mode='same')


im = diffread(Path("images") / "mnxc" / "Cr_1.tif")
mask = np.ones_like(im, dtype=bool)
mask[0:1250, 950:1250] = False

im = im[::2, ::2]
mask = mask[::2, ::2]

r, c = center_of_mass_masked(im, mask)
side_length = floor(min([r, abs(r - im.shape[0]), c, abs(c - im.shape[1])]))
rs = slice(r - side_length, r + side_length)
cs = slice(c - side_length, c + side_length)
im = im[rs, cs]
mask = mask[rs, cs]
im_r = im[::-1, ::-1]
mask_r = mask[::-1, ::-1]

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(6, 6))
xcorr = np.abs(correlate(im, im_r, mask, mask_r))
for ax, image in zip([ax1, ax2], [im, im_r]):
    ax.imshow(image, vmin=0, vmax=200, cmap="inferno")

ax3.imshow(xcorr, vmin=xcorr.mean(), cmap="inferno")
ax3.axhline(y=xcorr.shape[0] / 2, linestyle="--", color="w", linewidth=1)
ax3.axvline(x=xcorr.shape[1] / 2, linestyle="--", color="w", linewidth=1)

xcorr_zoomed = xcorr[
    xcorr.shape[0] // 4 : 3 * xcorr.shape[0] // 4,
    xcorr.shape[1] // 4 : 3 * xcorr.shape[1] // 4,
]
ax4.imshow(xcorr_zoomed, vmin=xcorr.mean(), cmap="inferno")
ax4.axhline(y=xcorr_zoomed.shape[0] / 2, linestyle="--", color="w", linewidth=1)
ax4.axvline(x=xcorr_zoomed.shape[1] / 2, linestyle="--", color="w", linewidth=1)
ax4.text(
    x=0.05, y=0.95, s="2x", transform=ax4.transAxes, ha="left", va="top", color="w"
)

shift, *_ = phase_cross_correlation(
    reference_image=im,
    moving_image=im_r,
    reference_mask=mask,
    moving_mask=mask_r,
)

ax4.arrow(
    x=xcorr_zoomed.shape[1] / 2,
    y=xcorr_zoomed.shape[0] / 2,
    dx=shift[1],
    dy=shift[0],
    color="w",
    length_includes_head=True,
    head_width=10,
)

for ax in (ax1, ax2, ax3, ax4):
    ax.axis("off")
plt.tight_layout()
```

The cross-correlation in the bottom right corner (zoomed by 2x) shows that the true center is the approximate center we found earlier, corrected by the small shift (white arrow)! For single-crystal diffraction patterns, the resulting is even more striking:

```{.python .matplotlib caption="**Top left**: diffraction pattern. **Top right**: radially-inverted diffraction pattern about an approximate center. **Bottom left**: masked normalized cross-correlation between the two diffraction patterns. **Bottom right**: 2x zoom on the cross-correlation shows the translation mismatch between the diffraction patterns."}
import numpy as np
import matplotlib.pyplot as plt
from skued import diffread
from pathlib import Path
from math import floor
from skimage.registration import phase_cross_correlation
from skimage.registration._masked_phase_cross_correlation import cross_correlate_masked


def center_of_mass_masked(im, mask):
    rr, cc = np.indices(im.shape)
    weights = im * mask.astype(im.dtype)

    r = np.average(rr, weights=weights)
    c = np.average(cc, weights=weights)
    return int(r), int(c)


def correlate(arr1, arr2, m1, m2):
    return cross_correlate_masked(arr1=arr1, arr2=arr2, m1=m1, m2=m2, mode='same')


im = diffread(Path("images") / "autocenter" / "graphite.tif")
mask = diffread(Path("images") / "autocenter" / "graphite_mask.tif").astype(bool)

r, c = center_of_mass_masked(im, mask)
side_length = floor(min([r, abs(r - im.shape[0]), c, abs(c - im.shape[1])]))
rs = slice(r - side_length, r + side_length)
cs = slice(c - side_length, c + side_length)
im = im[rs, cs]
mask = mask[rs, cs]
im_r = im[::-1, ::-1]
mask_r = mask[::-1, ::-1]

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(6, 6))
xcorr = np.abs(correlate(im, im_r, mask, mask_r))
for ax, image in zip([ax1, ax2], [im, im_r]):
    ax.imshow(image, vmin=0, vmax=200, cmap="inferno")

ax3.imshow(xcorr, vmin=xcorr.mean(), cmap="inferno")
ax3.axhline(y=xcorr.shape[0] / 2, linestyle="--", color="w", linewidth=1)
ax3.axvline(x=xcorr.shape[1] / 2, linestyle="--", color="w", linewidth=1)

xcorr_zoomed = xcorr[
    xcorr.shape[0] // 4 : 3 * xcorr.shape[0] // 4,
    xcorr.shape[1] // 4 : 3 * xcorr.shape[1] // 4,
]
ax4.imshow(xcorr_zoomed, vmin=xcorr.mean(), cmap="inferno")
ax4.axhline(y=xcorr_zoomed.shape[0] / 2, linestyle="--", color="w", linewidth=1)
ax4.axvline(x=xcorr_zoomed.shape[1] / 2, linestyle="--", color="w", linewidth=1)
ax4.text(
    x=0.05, y=0.95, s="2x", transform=ax4.transAxes, ha="left", va="top", color="w"
)

shift, *_ = phase_cross_correlation(
    reference_image=im,
    moving_image=im_r,
    reference_mask=mask,
    moving_mask=mask_r,
)

ax4.arrow(
    x=xcorr_zoomed.shape[1] / 2,
    y=xcorr_zoomed.shape[0] / 2,
    dx=shift[1],
    dy=shift[0],
    color="w",
    length_includes_head=True,
    head_width=10,
)

for ax in (ax1, ax2, ax3, ax4):
    ax.axis("off")
plt.tight_layout()
```

We can put the two steps together and determine a pixel-perfect center:

```{.python .matplotlib}
from skued import diffread, autocenter
from pathlib import Path

im1 = diffread(Path("images") / "autocenter" / "graphite.tif")
mask1 = diffread(Path("images") / "autocenter" / "graphite_mask.tif")

im2 = diffread(Path("images") / "mnxc" / "Cr_1.tif")
mask2 = np.ones_like(im2, dtype=bool)
mask2[0:1250, 950:1250] = False

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(6,3))

for im, mask, ax in zip([im1, im2], [mask1, mask2], [ax1, ax2]):

    r, c = autocenter(im=im, mask=mask)
    ax.imshow(im, vmin=0, vmax=200, cmap='inferno')
    ax.scatter(c, r, color='r')
    ax.axhline(y=r, linestyle='dashed', linewidth=1, color='w')
    ax.axvline(x=c, linestyle='dashed', linewidth=1, color='w')
    ax.axis('off')

plt.tight_layout()
```

## Bonus: low-quality diffraction

Here's a fun consequence: the technique works also for diffraction patterns that are pretty crappy and very far off center, provided that the asymmetry in the background is taken care-of:

```{.python .matplotlib}
from skued import diffread, autocenter
from pathlib import Path

im = diffread(Path("images") / "autocenter" / "ewald_walkoff.tif")
mask = diffread(Path("images") / "autocenter" / "ewald_walkoff_mask.tif")

fig, ax = plt.subplots(1, 1, figsize=(3,3))

r, c = autocenter(im=im, mask=mask)
ax.imshow(im, vmin=0, vmax=1400, cmap='inferno')
ax.scatter(c, r, color='r')
ax.axhline(y=r, linestyle='dashed', linewidth=1, color='w')
ax.axvline(x=c, linestyle='dashed', linewidth=1, color='w')
ax.axis('off')

plt.tight_layout()
```

## Conclusion 

In this post, we have determined a robust way to compute the center of a diffraction pattern without any parameters, by making use of a strong invariant: radial inversion symmetry. My favourite part: this method admits no free parameters!

If you want to make use of this, [take a look at `autocenter`](https://scikit-ued.readthedocs.io/en/master/functions/skued.autocenter.html#skued.autocenter), a new function that has been added to [scikit-ued](https://github.com/LaurentRDC/scikit-ued).


[^1]: L.P. René de Cotret *et al*, _Time- and momentum-resolved phonon population dynamics with ultrafast electron diffuse scattering_, Phys. Rev. B __100__ (2019) [DOI: 10.1103/PhysRevB.100.214115](https://journals.aps.org/prb/abstract/10.1103/PhysRevB.100.214115).

[^2]: Liu, Lai Chung. [Chemistry in Action: Making Molecular Movies with Ultrafast Electron Diffraction and Data Science](http://hdl.handle.net/1807/97517). University of Toronto, 2019.

[^3]: Dirk Padfield. *Masked object registration in the Fourier domain*. IEEE Transactions on Image Processing, **21**(5):2706–2718, 2012. [DOI: 10.1109/TIP.2011.2181402](https://doi.org/10.1109/TIP.2011.2181402)

[^4]: Dirk Padfield. *Masked FFT registration*. Prov. Computer Vision and Pattern Recognition. pp 2918-2925 (2010). [DOI:10.1109/CVPR.2010.5540032](https://doi.org/10.1109/CVPR.2010.5540032)
