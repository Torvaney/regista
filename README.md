README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
regista
=======

[![Build Status](https://travis-ci.org/Torvaney/regista.svg?branch=master)](https://travis-ci.org/Torvaney/regista) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Torvaney/regista?branch=master&svg=true)](https://ci.appveyor.com/project/Torvaney/regista) [![Coverage status](https://codecov.io/gh/Torvaney/regista/branch/master/graph/badge.svg)](https://codecov.io/github/Torvaney/regista?branch=master) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Overview
--------

regista is a package for performing some of the common modelling tasks in soccer analytics. Currently a *work in progress*.

Installation
------------

regista is not currently available on CRAN but can be downloaded from github like so:

``` r
# install.packages("devtools")
devtools::install_github("torvaney/regista")
```

Examples
--------

### Dixon-Coles

The ["Dixon-Coles model"](http://web.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf) is a modified poisson model, specifically designed for estimating teams' strengths and for predicting football matches.

Regista provides an implementation of this model:

``` r
library(regista)

fit <- dixoncoles(hgoal, agoal, home, away, data = premier_league_2010)

print(fit)
```

    ## 
    ## Dixon-Coles model with specification:
    ## 
    ## Home goals: hgoal ~ off(home) + def(away) + hfa + 0
    ## Away goals: agoal ~ off(away) + def(home) + 0
    ## Weights   : 1

The Dixon-Coles model provides estimates of each team's offensive and defensive strength, along with an estimate of home-field advantage (`hfa`):

``` r
parameters <- tibble::tibble(
  parameter = names(fit$par),
  value     = fit$par
)

parameters
```

    ## # A tibble: 42 x 2
    ##    parameter                 value
    ##    <chr>                     <dbl>
    ##  1 off___Arsenal           0.369  
    ##  2 off___Aston Villa      -0.0295 
    ##  3 off___Birmingham City  -0.273  
    ##  4 off___Blackburn Rovers -0.0608 
    ##  5 off___Blackpool         0.142  
    ##  6 off___Bolton Wanderers  0.0420 
    ##  7 off___Chelsea           0.304  
    ##  8 off___Everton           0.0118 
    ##  9 off___Fulham           -0.00836
    ## 10 off___Liverpool         0.162  
    ## # ... with 32 more rows

Regista also comes with a `predict` method to predict method, to either predict the goalscoring rate of either team, or the probabilities of different possible scorelines:

``` r
with_predictions <- cbind(
  premier_league_2010,
  predict(fit, premier_league_2010, type = "rates")
)

tibble::as_tibble(with_predictions[, c("date", "home", "away", "home_rate", "away_rate")])
```

    ## # A tibble: 380 x 5
    ##    date       home    away             home_rate away_rate
    ##    <chr>      <fct>   <fct>                <dbl>     <dbl>
    ##  1 2011-05-15 Arsenal Aston Villa           2.40     0.873
    ##  2 2010-10-16 Arsenal Birmingham City       2.35     0.685
    ##  3 2011-04-02 Arsenal Blackburn Rovers      2.42     0.846
    ##  4 2010-08-21 Arsenal Blackpool             3.23     1.04 
    ##  5 2010-09-11 Arsenal Bolton Wanderers      2.30     0.938
    ##  6 2010-12-27 Arsenal Chelsea               1.38     1.22 
    ##  7 2011-02-01 Arsenal Everton               1.85     0.910
    ##  8 2010-12-04 Arsenal Fulham                1.79     0.892
    ##  9 2011-04-17 Arsenal Liverpool             1.80     1.06 
    ## 10 2011-01-05 Arsenal Manchester City       1.36     1.07 
    ## # ... with 370 more rows

A more flexible api is provided with `dixoncoles_ext`, which allows the base Dixon-Coles model to be extended arbitrarily.

There are some more extensive examples and analyses using regista available at the following links:

-   [Modelling the World Cup with regista](http://www.statsandsnakeoil.com/2018/06/05/modelling-the-world-cup-with-regista/)
-   [Dixon Coles and xG: together at last](http://www.statsandsnakeoil.com/2018/06/22/dixon-coles-and-xg-together-at-last/)
-   [What a diff'rence xG makes](http://www.statsandsnakeoil.com/2018/07/15/what-a-diff-rence-xg-makes/)
