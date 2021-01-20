
# neuronsim <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/neuronsim)](https://cran.r-project.org/package=neuronsim)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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

``` r
library(neuronsim)

init_state <- c(r = 0, v = -2)
params <- c(delta = 1, etabar = -2.5, J = 10.5)
times_seq <- seq(from = -10, to = 80, by = 0.0001)
current <- sin_input(t, current = 3, frequency = pi / 20)
```

The macroscopic dynamics of neuronal ensembles can be described by
solving the firing-rate equations (FREs):

``` r
fre_output <- solve_fre(
  params = params,
  init_state = init_state,
  times = times_seq,
  input = current,
  method = "rk4"
)
```

The microscopic dynamics of neuronal ensembles can be described by
running a QIF neurons simulation:

``` r
qif_output <- simulate_qif(
  params = params,
  init_state = init_state,
  times = times_seq,
  input = current(times_seq)
)
```

To plot the macroscopic and microscopic dynamics of the ensemble we can
run:

``` r
list(fre_output, qif_output$data) %>% 
  plot_dynamics()
```

![](https://raw.githubusercontent.com/aldomann/neuronsim/main/man/figures/README-sin_dynamics-1.png)
