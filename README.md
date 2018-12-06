
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

The Dixon-Coles model provides estimates of each team’s offensive and
defensive strength, along with an estimate of home-field advantage
(`hfa`):

``` r
parameters <- tibble::tibble(
  parameter = names(fit$par),
  value     = fit$par
)

parameters
#> # A tibble: 42 x 2
#>    parameter                value
#>    <chr>                    <dbl>
#>  1 off___Arsenal           0.297 
#>  2 off___Aston Villa      -0.102 
#>  3 off___Birmingham City  -0.345 
#>  4 off___Blackburn Rovers -0.133 
#>  5 off___Blackpool         0.0696
#>  6 off___Bolton Wanderers -0.0303
#>  7 off___Chelsea           0.232 
#>  8 off___Everton          -0.0604
#>  9 off___Fulham           -0.0807
#> 10 off___Liverpool         0.0901
#> # ... with 32 more rows
```

Regista also comes with a `predict` method, to predict the goalscoring
rate of either team, or the probabilities of different possible
scorelines or match outcomes:

``` r
# Create a copy of the original data and attach predictions
to_predict <- premier_league_2010
to_predict$predictions <- predict(fit, newdata = premier_league_2010)

to_predict
#> # A tibble: 380 x 8
#>    date      home    away          hgoal agoal result hfa   predictions   
#>    <chr>     <fct>   <fct>         <dbl> <dbl> <fct>  <lgl> <list>        
#>  1 2011-05-… Arsenal Aston Villa       1     2 A      TRUE  <tibble [2 × …
#>  2 2010-10-… Arsenal Birmingham C…     2     1 H      TRUE  <tibble [2 × …
#>  3 2011-04-… Arsenal Blackburn Ro…     0     0 D      TRUE  <tibble [2 × …
#>  4 2010-08-… Arsenal Blackpool         6     0 H      TRUE  <tibble [2 × …
#>  5 2010-09-… Arsenal Bolton Wande…     4     1 H      TRUE  <tibble [2 × …
#>  6 2010-12-… Arsenal Chelsea           3     1 H      TRUE  <tibble [2 × …
#>  7 2011-02-… Arsenal Everton           2     1 H      TRUE  <tibble [2 × …
#>  8 2010-12-… Arsenal Fulham            2     1 H      TRUE  <tibble [2 × …
#>  9 2011-04-… Arsenal Liverpool         1     1 D      TRUE  <tibble [2 × …
#> 10 2011-01-… Arsenal Manchester C…     0     0 D      TRUE  <tibble [2 × …
#> # ... with 370 more rows
```

The regista package is designed to work fluidly with the tidyverse and
tidy principles. For instance, predictions can be handled easily with
the [broom
package](https://www.tidyverse.org/articles/2018/07/broom-0-5-0/).

To get predictions of Home/Draw/Away probabilities as columns in a
dataframe, you can use the `broom::augment` function:

``` r
library(tidyverse)
library(broom)

fit %>% 
  augment(newdata = premier_league_2010, type = "outcomes") %>% 
  unnest() %>%
  mutate(prob = scales::percent(prob, 2)) %>%  # Prettify output
  spread(outcome, prob) %>% 
  select(home, away, home_win, draw, away_win, result)
#> # A tibble: 380 x 6
#>    home                  away               home_win draw  away_win result
#>    <fct>                 <fct>              <chr>    <chr> <chr>    <fct> 
#>  1 Aston Villa           West Ham United    56%      24%   20%      H     
#>  2 Blackburn Rovers      Everton            34%      30%   36%      H     
#>  3 Bolton Wanderers      Fulham             38%      30%   30%      D     
#>  4 Chelsea               West Bromwich Alb… 78%      16%   8%       H     
#>  5 Sunderland            Birmingham City    50%      30%   20%      D     
#>  6 Tottenham Hotspur     Manchester City    32%      32%   36%      D     
#>  7 Wigan Athletic        Blackpool          46%      26%   28%      A     
#>  8 Wolverhampton Wander… Stoke City         36%      30%   34%      H     
#>  9 Liverpool             Arsenal            40%      28%   32%      D     
#> 10 Manchester United     Newcastle United   72%      18%   10%      H     
#> # ... with 370 more rows
```

Or to get model parameters in a table format:

``` r
tidy(fit)
#> # A tibble: 42 x 3
#>    parameter team               value
#>    <chr>     <chr>              <dbl>
#>  1 off       Arsenal           0.297 
#>  2 off       Aston Villa      -0.102 
#>  3 off       Birmingham City  -0.345 
#>  4 off       Blackburn Rovers -0.133 
#>  5 off       Blackpool         0.0696
#>  6 off       Bolton Wanderers -0.0303
#>  7 off       Chelsea           0.232 
#>  8 off       Everton          -0.0604
#>  9 off       Fulham           -0.0807
#> 10 off       Liverpool         0.0901
#> # ... with 32 more rows
```

(Note that team parameter estimates are in log space).

A more flexible api is provided with `dixoncoles_ext`, which allows the
base Dixon-Coles model to be extended arbitrarily.

There are some more extensive examples and analyses using regista
available at the following links:

  - [Modelling the World Cup with
    regista](http://www.statsandsnakeoil.com/2018/06/05/modelling-the-world-cup-with-regista/)
  - [Dixon Coles and xG: together at
    last](http://www.statsandsnakeoil.com/2018/06/22/dixon-coles-and-xg-together-at-last/)
  - [What a diff’rence xG
    makes](http://www.statsandsnakeoil.com/2018/07/15/what-a-diff-rence-xg-makes/)

## Other options

  - The [goalmodel](https://github.com/opisthokonta/goalmodel) R package
    contains an implementation of the Dixon-Coles model, along with some
    additional method for modelling the number of goals scored in sports
    games.
