---
title: The masked normalized cross-correlation and its application to image registration
date: 2019-04-30
updated: 2021-12-06
summary: "For my first contribution to open-source library scikit-image, I implemented the masked normalized cross-correlation. This post details the why and how this happened."
---

Image registration consists in determinining the most likely transformation between two images --- most importantly translation, which is what I am most concerned with.

How can we detect the translation between two otherwise similar image? This is an application of **cross-correlation**. The cross-correlation of two images is the degree of similitude between images for every possible translation between them. Mathematically, given grayscale images as discrete functions $I_1(i,j)$ and $I_2(i,j)$, their cross-correlation $I_1 \star I_2$ is defined as:
$$
    (I_1 \star I_2)(u, v) \equiv \sum_{i,j} I_1(i, j) \cdot I_2(i - u, j - v)
$$

For example, if $I_1 = I_2$, then $I_1 \star I_2$ has its maximum at $(u,v) =$ (0,0). What happens if $I_1$ and $I_2$ are shifted from each other? Let's see:

```{.python .matplotlib caption="The cross-correlation between shifted images exhibits a global maxima at the location corresponding to relative translation."}
from skimage import data
from scipy.ndimage import fourier_shift

image = data.camera()
shift = (-50, 0)

# The shift corresponds to the pixel offset relative to the reference image
offset_image = fourier_shift(np.fft.fftn(image), shift)
offset_image = np.fft.ifftn(offset_image)

fig = plt.figure(figsize=(8, 3))
ax1 = plt.subplot(1, 3, 1)
ax2 = plt.subplot(1, 3, 2, sharex=ax1, sharey=ax1)
ax3 = plt.subplot(1, 3, 3)

ax1.imshow(image, cmap='gray')
ax1.set_axis_off()
ax1.set_title('Reference image')

ax2.imshow(offset_image.real, cmap='gray')
ax2.set_axis_off()
ax2.set_title('Offset image')

# Show the output of a cross-correlation to show what the algorithm is
# doing behind the scenes
image_product = np.fft.fft2(image) * np.fft.fft2(offset_image).conj()
cc_image = np.fft.fftshift(np.fft.ifft2(image_product))
ax3.imshow(cc_image.real, cmap='viridis')
ax3.set_axis_off()
ax3.set_title("Cross-correlation")
```

In the above example, the cross-correlation is maximal at (50, 0), which is exactly the translation required to *shift back* the second image to match the first one. Finding the translation between images is then a simple matter of determining the glocal maximum of the cross-correlation. This operation is so useful that it is implemented in the Python library [scikit-image](https://scikit-image.org) as [`skimage.feature.phase_cross_correlation`](https://scikit-image.org/docs/dev/api/skimage.registration.html?highlight=phase#skimage.registration.phase_cross_correlation).

It turns out that in my field of research, image registration can be crucial to correct experimental data. My primary research tool is [ultrafast electron diffraction](http://www.physics.mcgill.ca/siwicklab). Without knowing the details, you can think of this technique as a kind of microscope. A single image from one of our experiments looks like this:

![An electron diffraction pattern of polycrystalline chromium.](/images/mnxc/Cr_1.png)

Most of the electron beam is unperturbed by the sample; this is why we use a metal beam-block (seen as a black rod in the image above) to prevent the electrons from damaging our apparatus.

Our experiments are synthesized from hundreds of gigabytes of images like the one above, and it may take up to 72h (!) to take all the images we need. Over the course of this time, the electron beam may shift in a way that moves the image, but *not the beam-block*[^1]. Heres's what I mean:

```{.python .matplotlib caption="Here is the difference between two equivalent images, acquired a few hours apart. The shift between them is evident in the third panel."}

from skued import diffread
from pathlib import Path

ref = diffread(Path("images") / "mnxc" / "Cr_1.tif")
im = diffread(Path("images") / "mnxc" / "Cr_2.tif")

fig, (ax1, ax2, ax3) = plt.subplots(nrows=1, ncols=3, figsize=(9, 3))
ax1.imshow(ref, vmin=0, vmax=200, cmap='inferno')
ax2.imshow(im, vmin=0, vmax=200, cmap='inferno')
ax3.imshow((ref - im), cmap="RdBu_r")

for ax in (ax1, ax2, ax3):
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)

ax1.set_title("Reference")
ax2.set_title("Data")
ax3.set_title("Difference")

plt.tight_layout()
```

This does not fly. We need to be able to compare images together, and shifts by more than 1px are problematic. We need to correct for this shift, for every image, with respect to the first one. However, we are also in a bind, because unlike the example above, the images are not completely shifted; one part of them, the beam-block, is *static*, while the image behind it shifts. 

The crux of the problem is this: the cross-correlation between images gives us the shift between them. However, it is not immediately obvious how to tell the cross-correlation operation to ignore *certain parts* of the image. Is there some kind of operation, similar to the cross-correlation, that allows to mask parts of the images we want to ignore? 

Thanks to the work of Dr. Dirk Padfield[^2] [^3], we now know that such an operation exists: the **masked normalized cross-correlation**. [In his 2012 article](https://doi.org/10.1109/TIP.2011.2181402), he explains the procedure and performance of this method to register images with masks. One such example is the registration of ultrasound images; [unfortunately, showing you the figure from the article would cost me 450 $US](/images/mnxc/criminal.png), so you'll have to go look at it yourselves.

<hr>

In order to fix our registration problem, then, I implemented the masked normalized cross-correlation operation --- and its associated registration function --- in our ultrafast electron diffraction toolkit, [scikit-ued](https://scikit-ued.rtfd.io)[^4]. Here's an example of it in action:

```{.python .matplotlib caption="Using the masked-normalized cross-correlation to align two diffraction patterns of polycrystalline chromium. The mask shown tells the algorithm to ignore the beam-block of both images."}
from skued import diffread, align
from pathlib import Path

ref = diffread(Path("images") / "mnxc" / "Cr_1.tif")
im = diffread(Path("images") / "mnxc" / "Cr_2.tif")

mask = np.ones_like(ref, dtype=np.bool)
mask[0:1250, 950:1250] = False

shifted = align(image=im, reference=ref, mask=mask)

fig, ((ax1, ax2, ax3), (ax4, ax5, ax6)) = plt.subplots(nrows=2, ncols=3, figsize=(9, 6))
ax1.imshow(ref, vmin=0, vmax=200, cmap='inferno')
ax2.imshow(im, vmin=0, vmax=200, cmap='inferno')
ax3.imshow(ref - im, cmap="RdBu_r")
ax4.imshow(mask * im, vmin=0, vmax=200, cmap="inferno")
ax5.imshow(shifted, vmin=0, vmax=200, cmap='inferno')
ax6.imshow(ref - shifted, cmap="RdBu_r")

for ax in (ax1, ax2, ax3, ax4, ax5, ax6):
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)

ax1.set_title("Reference")
ax2.set_title("Data")
ax3.set_title("Difference")
ax4.set_title("Masked image")
ax5.set_title("Aligned data")
ax6.set_title("Difference after shift")

plt.tight_layout()
```

## Contributing to scikit-image

However, since this tool could see use in a more general setting, I decided to contribute it to [scikit-image](https://scikit-image.org/):

1. My contribution starts by bringing up the subject via a GitHub issue ([issue #3330](https://github.com/scikit-image/scikit-image/issues/3330)). 
2. I forked scikit-image and integrated the code and tests from scikit-ued to scikit-image. The changes are visible in the [pull request #3334](https://github.com/scikit-image/scikit-image/pull/3334). 
3. Finally, some documentation improvements and an additional gallery example were added in [pull request #3528](https://github.com/scikit-image/scikit-image/pull/3528).

In the end, **a new function has been added, [`skimage.registration.phase_cross_correlation`](https://scikit-image.org/docs/stable/api/skimage.registration.html#skimage.registration.phase_cross_correlation)** (previously `skimage.feature.masked_register_translation`).

[^1]: Technically, the rotation of the electron beam about its source will also move the shadow of the beam-block. However, because the beam-block is much closer to the electron source, the effect is imperceptible.

[^2]: Dirk Padfield. *Masked object registration in the Fourier domain*. IEEE Transactions on Image Processing, **21**(5):2706–2718, 2012. [DOI: 10.1109/TIP.2011.2181402](https://doi.org/10.1109/TIP.2011.2181402)

[^3]: Dirk Padfield. *Masked FFT registration*. Prov. Computer Vision and Pattern Recognition. pp 2918-2925 (2010). [DOI:10.1109/CVPR.2010.5540032](https://doi.org/10.1109/CVPR.2010.5540032)

[^4]: L. P. René de Cotret et al, _An open-source software ecosystem for the interactive exploration of ultrafast electron scattering data_, Advanced Structural and Chemical Imaging __4__:11 (2018) [DOI:10.1186/s40679-018-0060-y](https://ascimaging.springeropen.com/articles/10.1186/s40679-018-0060-y). This publication is open-access<i class="ai ai-open-access"></i> .