---
name: crystals
repository: https://github.com/LaurentRDC/crystals
---

<a href="https://pypi.org/pypi/crystals" target="_blank">
    <img src="https://img.shields.io/pypi/v/crystals.svg">
</a> 
<a href="https://anaconda.org/conda-forge/crystals" target="_blank">
    <img src="https://img.shields.io/conda/vn/conda-forge/crystals.svg">
</a>

`crystals` is a fully-tested Python package containing data structures representing abstract crystals. Parsing from structure files (e.g. CIF, PDB) is made easy, as is symmetry-determination through the [SPGLIB](https://atztogo.github.io/spglib/) package.

Here's a quick example:

```
>>> from crystals import Crystal
>>>
>>> vo2 = Crystal.from_database('vo2-m1')
>>> print(vo2)	   # Short string representation
< Crystal object with following unit cell:
    Atom O  @ (0.90, 0.79, 0.80)
    Atom O  @ (0.90, 0.71, 0.30)
    Atom O  @ (0.61, 0.31, 0.71)
    Atom O  @ (0.39, 0.69, 0.29)
    Atom O  @ (0.61, 0.19, 0.21)
    Atom O  @ (0.10, 0.29, 0.70)
    Atom O  @ (0.10, 0.21, 0.20)
    Atom O  @ (0.39, 0.81, 0.79)
    Atom V  @ (0.76, 0.03, 0.97)
    Atom V  @ (0.76, 0.48, 0.47)
    ... omitting 2 atoms ...
Lattice parameters:
    a=5.743Å, b=4.517Å, c=5.375Å
    α=90.000°, β=122.600°, γ=90.000°
Chemical composition:
    O: 66.667%
    V: 33.333%
Source:
    (...omitted...)\crystals\cifs\vo2-m1.cif >
>>>
>>> print(vo2.symmetry())
{'international_symbol': 'P2_1/c', 
'hall_symbol': '-P 2ybc', 
'hm_symbol': 'P121/c1',
'international_number': 14, 
'hall_number': 81, 
'international_full': 'P 1 2_1/c 1', 
'pointgroup': 'C2h'}
```

To install from PyPI:

    > python -m pip install crystals

For Anaconda users, `crystals` is also available on the `conda-forge` channel:

    > conda install -c conda-forge crystals