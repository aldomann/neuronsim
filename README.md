
# neuronsim <img src="figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/neuronsim)](https://cran.r-project.org/package=neuronsim)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- [![R build status](https://github.com/aldomann/<package>/workflows/R-CMD-check/badge.svg)](https://github.com/aldomann/<package>/actions) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/aldomann/<package>/branch/master/graph/badge.svg)](https://codecov.io/gh/aldomann/<package>?branch=master) -->
<!-- [![pkgdown status](https://github.com/aldomann/<package>/workflows/pkgdown/badge.svg)](https://aldomann.github.io/<package>/) -->
<!-- badges: end -->

## Overview

The goal of `{neuronsim}` is to simulate the dynamics of neuronal
ensembles using the model of FREs and QIF neurons.

## Installation

<!-- You can install the released version of neuronsim from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("neuronsim") -->
<!-- ``` -->
<!-- And  -->

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aldomann/neuronsim")
```

## Examples

The macroscopic dynamics of neuronal ensembles can be described by
solving the firing-rate equations (FREs):

``` r
library(neuronsim)

fre_output <- solve_fre(
  params = c(delta = 1, etabar = -2.5, J = 10.5),
  init_state = c(r = 0, v = -2),
  times = seq(from = -10, to = 80, by = 0.01),
  input = sin_input(t, current = 3, frequency = pi/20),
  method = "rk4"
)
```

To plot the macroscopic dynamics described by the FREs we can run

``` r
plot_macro_dynamics(fre_output)
```

<img src="man/figures/README-sin_macro_dynamics-1.png" style="display: block; margin: auto;" />
