
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regista <img src="man/figures/logo.png" align="right" />

[![Build
Status](https://travis-ci.org/Torvaney/regista.svg?branch=master)](https://travis-ci.org/Torvaney/regista)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/Torvaney/regista?branch=master&svg=true)](https://ci.appveyor.com/project/Torvaney/regista)
[![Coverage
status](https://codecov.io/gh/Torvaney/regista/branch/master/graph/badge.svg)](https://codecov.io/github/Torvaney/regista?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Overview

regista is a package for performing some of the common modelling tasks
in soccer analytics.

## Installation

regista is not currently available on CRAN but can be downloaded from
github like so:

``` r
# install.packages("devtools")
devtools::install_github("torvaney/regista")
```

## Examples

### Dixon-Coles

The [“Dixon-Coles
model”](http://web.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf) is
a modified poisson model, specifically designed for estimating teams’
strengths and for predicting football matches.

Regista provides an implementation of this model:

``` r
library(regista)

fit <- dixoncoles(hgoal, agoal, home, away, data = premier_league_2010)

print(fit)
#> 
#> Dixon-Coles model with specification:
#> 
#> Home goals: hgoal ~ off(home) + def(away) + hfa + 0
#> Away goals: agoal ~ off(away) + def(home) + 0
#> Weights   : 1
```

A more flexible api is provided with `dixoncoles_ext`, which allows the
base Dixon-Coles model to be extended arbitrarily.

`vignette("using-dixon-coles")` gives some simple examples for using the
model. Additionally, there are more extensive examples and analyses
using regista available at the following links:

  - [Modelling the World Cup with
    regista](http://www.statsandsnakeoil.com/2018/06/05/modelling-the-world-cup-with-regista/)
  - [Dixon Coles and xG: together at
    last](http://www.statsandsnakeoil.com/2018/06/22/dixon-coles-and-xg-together-at-last/)
  - [What a diff’rence xG
    makes](http://www.statsandsnakeoil.com/2018/07/15/what-a-diff-rence-xg-makes/)

## Other options

  - The [mezzala](https://github.com/Torvaney/mezzala) package provides
    similar functionality for Python.
  - The [goalmodel](https://github.com/opisthokonta/goalmodel) R package
    contains an implementation of the Dixon-Coles model, along with some
    additional method for modelling the number of goals scored in sports
    games.
