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
```

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

    ## Warning in log(.tau(hg, ag, home_rates, away_rates, rho)): NaNs produced

``` r
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
cbind(
  premier_league_2010,
  predict(fit, premier_league_2010, type = "rates")
)
```

    ##           date                    home                    away hgoal agoal
    ## 1   2011-05-15                 Arsenal             Aston Villa     1     2
    ## 2   2010-10-16                 Arsenal         Birmingham City     2     1
    ## 3   2011-04-02                 Arsenal        Blackburn Rovers     0     0
    ## 4   2010-08-21                 Arsenal               Blackpool     6     0
    ## 5   2010-09-11                 Arsenal        Bolton Wanderers     4     1
    ## 6   2010-12-27                 Arsenal                 Chelsea     3     1
    ## 7   2011-02-01                 Arsenal                 Everton     2     1
    ## 8   2010-12-04                 Arsenal                  Fulham     2     1
    ## 9   2011-04-17                 Arsenal               Liverpool     1     1
    ## 10  2011-01-05                 Arsenal         Manchester City     0     0
    ## 11  2011-05-01                 Arsenal       Manchester United     1     0
    ## 12  2010-11-07                 Arsenal        Newcastle United     0     1
    ## 13  2011-02-23                 Arsenal              Stoke City     1     0
    ## 14  2011-03-05                 Arsenal              Sunderland     0     0
    ## 15  2010-11-20                 Arsenal       Tottenham Hotspur     2     3
    ## 16  2010-09-25                 Arsenal    West Bromwich Albion     2     3
    ## 17  2010-10-30                 Arsenal         West Ham United     1     0
    ## 18  2011-01-22                 Arsenal          Wigan Athletic     3     0
    ## 19  2011-02-12                 Arsenal Wolverhampton Wanderers     2     0
    ## 20  2010-11-27             Aston Villa                 Arsenal     2     4
    ## 21  2010-10-31             Aston Villa         Birmingham City     0     0
    ## 22  2011-02-26             Aston Villa        Blackburn Rovers     4     1
    ## 23  2010-11-10             Aston Villa               Blackpool     3     2
    ## 24  2010-09-18             Aston Villa        Bolton Wanderers     1     1
    ## 25  2010-10-16             Aston Villa                 Chelsea     0     0
    ## 26  2010-08-29             Aston Villa                 Everton     1     0
    ## 27  2011-02-05             Aston Villa                  Fulham     2     2
    ## 28  2011-05-22             Aston Villa               Liverpool     1     0
    ## 29  2011-01-22             Aston Villa         Manchester City     1     0
    ## 30  2010-11-13             Aston Villa       Manchester United     2     2
    ## 31  2011-04-10             Aston Villa        Newcastle United     1     0
    ## 32  2011-04-23             Aston Villa              Stoke City     1     1
    ## 33  2011-01-05             Aston Villa              Sunderland     0     1
    ## 34  2010-12-26             Aston Villa       Tottenham Hotspur     1     2
    ## 35  2010-12-11             Aston Villa    West Bromwich Albion     2     1
    ## 36  2010-08-14             Aston Villa         West Ham United     3     0
    ## 37  2011-05-07             Aston Villa          Wigan Athletic     1     1
    ## 38  2011-03-19             Aston Villa Wolverhampton Wanderers     0     1
    ## 39  2011-01-01         Birmingham City                 Arsenal     0     3
    ## 40  2011-01-16         Birmingham City             Aston Villa     1     1
    ## 41  2010-08-21         Birmingham City        Blackburn Rovers     2     1
    ## 42  2010-10-23         Birmingham City               Blackpool     2     0
    ## 43  2011-04-02         Birmingham City        Bolton Wanderers     2     1
    ## 44  2010-11-20         Birmingham City                 Chelsea     1     0
    ## 45  2010-10-02         Birmingham City                 Everton     0     2
    ## 46  2011-05-15         Birmingham City                  Fulham     0     2
    ## 47  2010-09-12         Birmingham City               Liverpool     0     0
    ## 48  2011-02-02         Birmingham City         Manchester City     2     2
    ## 49  2010-12-28         Birmingham City       Manchester United     1     1
    ## 50  2011-02-15         Birmingham City        Newcastle United     0     2
    ## 51  2011-02-12         Birmingham City              Stoke City     1     0
    ## 52  2011-04-16         Birmingham City              Sunderland     2     0
    ## 53  2010-12-04         Birmingham City       Tottenham Hotspur     1     1
    ## 54  2011-03-05         Birmingham City    West Bromwich Albion     1     3
    ## 55  2010-11-06         Birmingham City         West Ham United     2     2
    ## 56  2010-09-25         Birmingham City          Wigan Athletic     0     0
    ## 57  2011-05-01         Birmingham City Wolverhampton Wanderers     1     1
    ## 58  2010-08-28        Blackburn Rovers                 Arsenal     1     2
    ## 59  2010-11-21        Blackburn Rovers             Aston Villa     2     0
    ## 60  2011-04-09        Blackburn Rovers         Birmingham City     1     1
    ## 61  2011-03-19        Blackburn Rovers               Blackpool     2     2
    ## 62  2011-04-30        Blackburn Rovers        Bolton Wanderers     1     0
    ## 63  2010-10-30        Blackburn Rovers                 Chelsea     1     2
    ## 64  2010-08-14        Blackburn Rovers                 Everton     1     0
    ## 65  2010-09-18        Blackburn Rovers                  Fulham     1     1
    ## 66  2011-01-05        Blackburn Rovers               Liverpool     3     1
    ## 67  2011-04-25        Blackburn Rovers         Manchester City     0     1
    ## 68  2011-05-14        Blackburn Rovers       Manchester United     1     1
    ## 69  2011-02-12        Blackburn Rovers        Newcastle United     0     0
    ## 70  2010-12-26        Blackburn Rovers              Stoke City     0     2
    ## 71  2010-10-18        Blackburn Rovers              Sunderland     0     0
    ## 72  2011-02-02        Blackburn Rovers       Tottenham Hotspur     0     1
    ## 73  2011-01-23        Blackburn Rovers    West Bromwich Albion     2     0
    ## 74  2010-12-18        Blackburn Rovers         West Ham United     1     1
    ## 75  2010-11-06        Blackburn Rovers          Wigan Athletic     2     1
    ## 76  2010-12-04        Blackburn Rovers Wolverhampton Wanderers     3     0
    ## 77  2011-04-10               Blackpool                 Arsenal     1     3
    ## 78  2011-02-12               Blackpool             Aston Villa     1     1
    ## 79  2011-01-04               Blackpool         Birmingham City     1     2
    ## 80  2010-09-25               Blackpool        Blackburn Rovers     1     2
    ## 81  2011-05-14               Blackpool        Bolton Wanderers     4     3
    ## 82  2011-03-07               Blackpool                 Chelsea     1     3
    ## 83  2010-11-06               Blackpool                 Everton     2     2
    ## 84  2010-08-28               Blackpool                  Fulham     2     2
    ## 85  2011-01-12               Blackpool               Liverpool     2     1
    ## 86  2010-10-17               Blackpool         Manchester City     2     3
    ## 87  2011-01-25               Blackpool       Manchester United     2     3
    ## 88  2011-04-23               Blackpool        Newcastle United     1     1
    ## 89  2011-04-30               Blackpool              Stoke City     0     0
    ## 90  2011-01-22               Blackpool              Sunderland     1     2
    ## 91  2011-02-22               Blackpool       Tottenham Hotspur     3     1
    ## 92  2010-11-01               Blackpool    West Bromwich Albion     2     1
    ## 93  2011-02-02               Blackpool         West Ham United     1     3
    ## 94  2011-04-16               Blackpool          Wigan Athletic     1     3
    ## 95  2010-11-20               Blackpool Wolverhampton Wanderers     2     1
    ## 96  2011-04-24        Bolton Wanderers                 Arsenal     2     1
    ## 97  2011-03-05        Bolton Wanderers             Aston Villa     3     2
    ## 98  2010-08-29        Bolton Wanderers         Birmingham City     2     2
    ## 99  2010-12-12        Bolton Wanderers        Blackburn Rovers     2     1
    ## 100 2010-11-27        Bolton Wanderers               Blackpool     2     2
    ## 101 2011-01-24        Bolton Wanderers                 Chelsea     0     4
    ## 102 2011-02-13        Bolton Wanderers                 Everton     2     0
    ## 103 2010-08-14        Bolton Wanderers                  Fulham     0     0
    ## 104 2010-10-31        Bolton Wanderers               Liverpool     0     1
    ## 105 2011-05-22        Bolton Wanderers         Manchester City     0     2
    ## 106 2010-09-26        Bolton Wanderers       Manchester United     2     2
    ## 107 2010-11-20        Bolton Wanderers        Newcastle United     5     1
    ## 108 2010-10-16        Bolton Wanderers              Stoke City     2     1
    ## 109 2011-05-07        Bolton Wanderers              Sunderland     1     2
    ## 110 2010-11-06        Bolton Wanderers       Tottenham Hotspur     4     2
    ## 111 2010-12-26        Bolton Wanderers    West Bromwich Albion     2     0
    ##     result  hfa home_rate away_rate
    ## 1        A TRUE 2.3977931 0.8728759
    ## 2        H TRUE 2.3501390 0.6845069
    ## 3        D TRUE 2.4231372 0.8459820
    ## 4        H TRUE 3.2328425 1.0361241
    ## 5        H TRUE 2.3041189 0.9376241
    ## 6        H TRUE 1.3781287 1.2185764
    ## 7        H TRUE 1.8460192 0.9096767
    ## 8        H TRUE 1.7891437 0.8915654
    ## 9        D TRUE 1.7972116 1.0569956
    ## 10       D TRUE 1.3633668 1.0742353
    ## 11       H TRUE 1.5776459 1.4103038
    ## 12       A TRUE 2.3437737 1.0285413
    ## 13       H TRUE 1.9664822 0.8205673
    ## 14       D TRUE 2.2809552 0.8311008
    ## 15       A TRUE 1.9107281 0.9961448
    ## 16       A TRUE 2.9195463 1.0323788
    ## 17       H TRUE 2.8682544 0.7973202
    ## 18       H TRUE 2.4685171 0.7359276
    ## 19       H TRUE 2.6550793 0.8449964
    ## 20       A TRUE 1.2142229 1.7237162
    ## 21       D TRUE 1.5779810 0.9076101
    ## 22       H TRUE 1.6269951 1.1217153
    ## 23       H TRUE 2.1706649 1.3738310
    ## 24       D TRUE 1.5470812 1.2432265
    ## 25       D TRUE 0.9253329 1.6157504
    ## 26       H TRUE 1.2394941 1.2061703
    ## 27       D TRUE 1.2013055 1.1821559
    ## 28       H TRUE 1.2067226 1.4015052
    ## 29       H TRUE 0.9154212 1.4243638
    ## 30       D TRUE 1.0592970 1.8699681
    ## 31       H TRUE 1.5737071 1.3637767
    ## 32       D TRUE 1.3203779 1.0880171
    ## 33       A TRUE 1.5315282 1.1019838
    ## 34       A TRUE 1.2829423 1.3208211
    ## 35       H TRUE 1.9603048 1.3688649
    ## 36       H TRUE 1.9258653 1.0571931
    ## 37       D TRUE 1.6574651 0.9757906
    ## 38       A TRUE 1.7827307 1.1204085
    ## 39       A TRUE 0.9521903 1.6894588
    ## 40       D TRUE 1.2625404 1.1343729
    ## 41       H TRUE 1.2758851 1.0994222
    ## 42       H TRUE 1.7022295 1.3465272
    ## 43       H TRUE 1.2132169 1.2185184
    ## 44       H TRUE 0.7256436 1.5836387
    ## 45       A TRUE 0.9720079 1.1821986
    ## 46       A TRUE 0.9420605 1.1586615
    ## 47       D TRUE 0.9463086 1.3736514
    ## 48       D TRUE 0.7178708 1.3960558
    ## 49       D TRUE 0.8306979 1.8328040
    ## 50       A TRUE 1.2340968 1.3366727
    ## 51       H TRUE 1.0354368 1.0663936
    ## 52       H TRUE 1.2010203 1.0800828
    ## 53       D TRUE 1.0060799 1.2945709
    ## 54       A TRUE 1.5372657 1.3416598
    ## 55       D TRUE 1.5102583 1.0361822
    ## 56       D TRUE 1.2997796 0.9563976
    ## 57       D TRUE 1.3980126 1.0981413
    ## 58       A TRUE 1.1768120 1.7419355
    ## 59       H TRUE 1.5603736 1.1696079
    ## 60       D TRUE 1.5293625 0.9172033
    ## 61       D TRUE 2.1037854 1.3883521
    ## 62       H TRUE 1.4994147 1.2563671
    ## 63       A TRUE 0.8968229 1.6328285
    ## 64       H TRUE 1.2013045 1.2189192
    ## 65       D TRUE 1.1642925 1.1946509
    ## 66       H TRUE 1.1695427 1.4163188
    ## 67       A TRUE 0.8872165 1.4394190
    ## 68       D TRUE 1.0266595 1.8897332
    ## 69       D TRUE 1.5252202 1.3781915
    ## 70       A TRUE 1.2796963 1.0995172
    ## 71       D TRUE 1.4843409 1.1136315
    ## 72       A TRUE 1.2434141 1.3347819
    ## 73       H TRUE 1.8999066 1.3833335
    ## 74       D TRUE 1.8665282 1.0683673
    ## 75       H TRUE 1.6063975 0.9861045
    ## 76       H TRUE 1.7278037 1.1322510
    ## 77       A TRUE 1.4413111 2.3240133
    ## 78       D TRUE 1.9110817 1.5604392
    ## 79       A TRUE 1.8731005 1.2236922
    ## 80       A TRUE 1.9312814 1.5123611
    ## 81       H TRUE 1.8364218 1.6761895
    ## 82       A TRUE 1.0983919 2.1784476
    ## 83       D TRUE 1.4713085 1.6262281
    ## 84       D TRUE 1.4259778 1.5938505
    ## 85       H TRUE 1.4324080 1.8895899
    ## 86       A TRUE 1.0866264 1.9204092
    ## 87       A TRUE 1.2574105 2.5211985
    ## 88       D TRUE 1.8680273 1.8387221
    ## 89       D TRUE 1.5673196 1.4669272
    ## 90       A TRUE 1.8179599 1.4857580
    ## 91       H TRUE 1.5228827 1.7808069
    ## 92       H TRUE 2.3269278 1.8455823
    ## 93       A TRUE 2.2860473 1.4253685
    ## 94       A TRUE 1.9674499 1.3156170
    ## 95       H TRUE 2.1161432 1.5105992
    ## 96       H TRUE 1.3042916 1.6563760
    ## 97       H TRUE 1.7294030 1.1121598
    ## 98       D TRUE 1.6950325 0.8721527
    ## 99       H TRUE 1.7476824 1.0778934
    ## 100      D TRUE 2.3316805 1.3201597
    ## 101      A TRUE 0.9939723 1.5526281
    ## 102      H TRUE 1.3314373 1.1590490
    ## 103      D TRUE 1.2904159 1.1359727
    ## 104      A TRUE 1.2962349 1.3467528
    ## 105      A TRUE 0.9833253 1.3687184
    ## 106      D TRUE 1.1378736 1.7969143
    ## 107      H TRUE 1.6904416 1.3104982
    ## 108      H TRUE 1.4183210 1.0455117
    ## 109      A TRUE 1.6451339 1.0589328
    ## 110      H TRUE 1.3781084 1.2692208
    ## 111      H TRUE 2.1057163 1.3153876
    ##  [ reached getOption("max.print") -- omitted 269 rows ]

A more flexible api is provided with `dixoncoles_ext`, which allows the base Dixon-Coles model to be extended arbitrarily.

There are some more extensive examples and analyses using regista available at the following links:

-   [Modelling the World Cup with regista](http://www.statsandsnakeoil.com/2018/06/05/modelling-the-world-cup-with-regista/)
-   [Dixon Coles and xG: together at last](http://www.statsandsnakeoil.com/2018/06/22/dixon-coles-and-xg-together-at-last/)
-   [What a diff'rence xG makes](http://www.statsandsnakeoil.com/2018/07/15/what-a-diff-rence-xg-makes/)
