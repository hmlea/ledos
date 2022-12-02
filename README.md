
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LEDOS

<!-- badges: start -->
<!-- badges: end -->

#### [Web application link](https://hmlea.shinyapps.io/ledos/)

Leatherwood’s Electron Density Orbital Simulator (**LEDOS**) creates
pointillist representations of hydrogenic orbitals based on electron
density. Each “point” represents a possible location of an electron at
any given time in the atom; the darker a point is, the more likely an
electron is to be found there. The hydrogen wave function is calculated
using the [Boost
implementation](https://www.boost.org/doc/libs/1_80_0/libs/math/doc/html/math_toolkit/sf_poly/sph_harm.html)
of the spherical harmonics in my
[**wavefunction**](https://github.com/hmlea/wavefunction) package. A
Monte Carlo simulation uses the wave function to choose points where
there is a high probability of an electron existing based on the quantum
numbers of the orbital. All these points are then plotted resulting in
an electron density cloud that takes on the true shape of the orbital.

**LEDOS** was greatly inspired by Alvin Q. Meng and his own project,
[Evanescence](https://al2me6.github.io/evanescence)
([GitHub](https://github.com/al2me6/evanescence)). Further, Alvin
enthusiastically shared insight into his project and answered many of my
technical questions. I wouldn’t have been able to create **LEDOS**
without his help; please check out his extraordinary project at the link
above.

## Usage

**LEDOS** simulates orbital plots in real time using the quantum numbers
n, l, and m. On the left-hand selection pane, you can choose quantum
numbers that correspond to different orbitals. **LEDOS** will simulate
and electron cloud based on the number of points selected and will
create an interactive plot in the output pane. The data used to make the
plot can be exported as a CSV or and image of the plot can be exported
as a PNG.

## To Do

In the future, I would like to:

-   Allow the customization of more plot features
-   Rework the download buttons
-   Allow advanced options to create large data sets
-   Add support for more complex orbitals
-   Improve the simulation progress bar
-   Clean up and refactor general code
-   Enhance the site design
-   Optimize the $\Psi$ calculation in the **wavefunction** package

## References

1.  Meng 2022, [Evanescence](https://al2me6.github.io/evanescence)
    ([GitHub](https://github.com/al2me6/evanescence))
2.  Tully *et al.* 2013, [Interactive Web-Based Pointillist
    Visualization of Hydrogenic Orbitals Using
    Jmol](https://doi.org/10.1021/ed300393s)
3.  DeBruyne 2003, [Bra, Ket, Dirac, and all
    that…](https://faculty.washington.edu/seattle/physics441/441xxxindex.html)
4.  Lobo *et al.* 2019, [A smooth path to plot hydrogen atom via Monte
    Carlo method](https://doi.org/10.1590/1806-9126-RBEF-2019-0073)
5.  Blanco *et al.* 1997, [Evaluation of the rotation matrices in the
    basis of real spherical
    harmonics](https://doi.org/10.1016/S0166-1280(97)00185-1)