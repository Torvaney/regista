README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
regista
=======

[![Build Status](https://travis-ci.org/Torvaney/regista.svg?branch=master)](https://travis-ci.org/Torvaney/regista) [![Coverage status](https://codecov.io/gh/Torvaney/regista/branch/master/graph/badge.svg)](https://codecov.io/github/Torvaney/regista?branch=master)

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
parameters <- data.frame(parameter = names(fit$par),
                         value     = fit$par,
                         row.names = NULL)

head(parameters)
```

    ##                parameter       value
    ## 1          off___Arsenal  0.36878555
    ## 2      off___Aston Villa -0.02954271
    ## 3  off___Birmingham City -0.27263737
    ## 4 off___Blackburn Rovers -0.06083794
    ## 5        off___Blackpool  0.14190619
    ## 6 off___Bolton Wanderers  0.04201302

Regista also comes with a `predict` method to predict method, to either predict the goalscoring rate of either team, or the probabilities of different possible scorelines:

``` r
with_predictions <- cbind(
  premier_league_2010,
  predict(fit, premier_league_2010, type = "rates")
)

head(with_predictions[, c("date", "home", "away", "home_rate", "away_rate")])
```

    ##         date    home             away home_rate away_rate
    ## 1 2011-05-15 Arsenal      Aston Villa  2.397793 0.8728759
    ## 2 2010-10-16 Arsenal  Birmingham City  2.350139 0.6845069
    ## 3 2011-04-02 Arsenal Blackburn Rovers  2.423137 0.8459820
    ## 4 2010-08-21 Arsenal        Blackpool  3.232843 1.0361241
    ## 5 2010-09-11 Arsenal Bolton Wanderers  2.304119 0.9376241
    ## 6 2010-12-27 Arsenal          Chelsea  1.378129 1.2185764

A more flexible api is provided with `dixoncoles_ext`, which allows the base Dixon-Coles model to be extended arbitrarily.

There are some more extensive examples and analyses using regista available at the following links:

-   [Modelling the World Cup with regista](http://www.statsandsnakeoil.com/2018/06/05/modelling-the-world-cup-with-regista/)
-   [Dixon Coles and xG: together at last](http://www.statsandsnakeoil.com/2018/06/22/dixon-coles-and-xg-together-at-last/)
-   [What a diff'rence xG makes](http://www.statsandsnakeoil.com/2018/07/15/what-a-diff-rence-xg-makes/)
