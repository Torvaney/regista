README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# regista

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
```

    ## 
    ## Dixon-Coles model with specification:
    ## 
    ## Home goals: hgoal ~ off(home) + def(away) + hfa + 0
    ## Away goals: agoal ~ off(away) + def(home) + 0
    ## Weights   : 1

The Dixon-Coles model provides estimates of each team’s offensive and
defensive strength, along with an estimate of home-field advantage
(`hfa`):

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

Regista also comes with a `predict` method, to predict the goalscoring
rate of either team, or the probabilities of different possible
scorelines or match outcomes:

``` r
# Create a copy of the original data and attach predictions
to_predict <- premier_league_2010
to_predict$predictions <- predict(fit, newdata = premier_league_2010)

to_predict
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
    ## 112 2011-04-09        Bolton Wanderers         West Ham United     3     0
    ## 113 2011-01-05        Bolton Wanderers          Wigan Athletic     1     1
    ## 114 2011-02-02        Bolton Wanderers Wolverhampton Wanderers     1     0
    ## 115 2010-10-03                 Chelsea                 Arsenal     2     0
    ## 116 2011-01-02                 Chelsea             Aston Villa     3     3
    ## 117 2011-04-20                 Chelsea         Birmingham City     3     1
    ## 118 2011-01-15                 Chelsea        Blackburn Rovers     2     0
    ## 119 2010-09-19                 Chelsea               Blackpool     4     0
    ## 120 2010-12-29                 Chelsea        Bolton Wanderers     1     0
    ## 121 2010-12-04                 Chelsea                 Everton     1     1
    ## 122 2010-11-10                 Chelsea                  Fulham     1     0
    ## 123 2011-02-06                 Chelsea               Liverpool     0     1
    ## 124 2011-03-20                 Chelsea         Manchester City     2     0
    ## 125 2011-03-01                 Chelsea       Manchester United     2     1
    ##     result  hfa                                     predictions
    ## 1        A TRUE home, away, 2.39779313861517, 0.872875854888291
    ## 2        H TRUE home, away, 2.35013896381262, 0.684506865904408
    ## 3        D TRUE home, away, 2.42313721339959, 0.845982018313218
    ## 4        H TRUE   home, away, 3.23284254509474, 1.0361241314697
    ## 5        H TRUE home, away, 2.30411887697941, 0.937624051033032
    ## 6        H TRUE   home, away, 1.3781287048521, 1.21857635165629
    ## 7        H TRUE home, away, 1.84601918117909, 0.909676732853366
    ## 8        H TRUE home, away, 1.78914367817553, 0.891565408361607
    ## 9        D TRUE  home, away, 1.79721155543208, 1.05699562335262
    ## 10       D TRUE  home, away, 1.36336679862669, 1.07423527237833
    ## 11       H TRUE  home, away, 1.57764586008588, 1.41030379068035
    ## 12       A TRUE  home, away, 2.34377366893105, 1.02854130298169
    ## 13       H TRUE home, away, 1.96648220829955, 0.820567269906587
    ## 14       D TRUE home, away, 2.28095524167717, 0.831100796139413
    ## 15       A TRUE  home, away, 1.9107281343739, 0.996144797349969
    ## 16       A TRUE    home, away, 2.91954625728687, 1.032378756173
    ## 17       H TRUE home, away, 2.86825437342552, 0.797320227626327
    ## 18       H TRUE home, away, 2.46851711341043, 0.735927648015898
    ## 19       H TRUE home, away, 2.65507934793693, 0.844996430515983
    ## 20       A TRUE  home, away, 1.21422291184042, 1.72371622648895
    ## 21       D TRUE home, away, 1.57798104234729, 0.907610134598035
    ## 22       H TRUE  home, away, 1.62699510310986, 1.12171534246671
    ## 23       H TRUE  home, away, 2.17066493837341, 1.37383101509286
    ## 24       D TRUE  home, away, 1.54708124207669, 1.24322652342729
    ## 25       D TRUE home, away, 0.925332928672142, 1.61575040607291
    ## 26       H TRUE  home, away, 1.23949405399601, 1.20617025638578
    ## 27       D TRUE  home, away, 1.20130547583296, 1.18215585641625
    ## 28       H TRUE  home, away, 1.20672258416521, 1.40150521165785
    ## 29       H TRUE home, away, 0.915421170886203, 1.42436382849871
    ## 30       D TRUE  home, away, 1.05929704459455, 1.86996811433337
    ## 31       H TRUE  home, away, 1.57370711863183, 1.36377669375958
    ## 32       D TRUE  home, away, 1.32037794044989, 1.08801709286385
    ## 33       A TRUE  home, away, 1.53152821396149, 1.10198384124604
    ## 34       A TRUE   home, away, 1.28294233640987, 1.3208211029518
    ## 35       H TRUE   home, away, 1.96030478077806, 1.3688648989785
    ## 36       H TRUE   home, away, 1.92586527672925, 1.0571930760074
    ## 37       D TRUE home, away, 1.65746505532262, 0.975790638400115
    ## 38       A TRUE  home, away, 1.78273069868831, 1.12040851923693
    ## 39       A TRUE  home, away, 0.95219029743878, 1.68945877823612
    ## 40       D TRUE  home, away, 1.26254038793242, 1.13437288812866
    ## 41       H TRUE  home, away, 1.27588512459664, 1.09942216873635
    ## 42       H TRUE  home, away, 1.70222952734181, 1.34652724885545
    ## 43       H TRUE  home, away, 1.21321689262322, 1.21851841449475
    ## 44       H TRUE home, away, 0.725643560165353, 1.58363869007527
    ## 45       A TRUE home, away, 0.972007858226916, 1.18219861041107
    ## 46       A TRUE  home, away, 0.942060479335241, 1.1586614769728
    ## 47       D TRUE home, away, 0.946308560922114, 1.37365144342928
    ## 48       D TRUE home, away, 0.717870786729518, 1.39605576398197
    ## 49       D TRUE home, away, 0.830697854679478, 1.83280402959205
    ## 50       A TRUE  home, away, 1.23409683243443, 1.33667274892404
    ## 51       H TRUE  home, away, 1.03543678149092, 1.06639364424499
    ## 52       H TRUE  home, away, 1.20102025037355, 1.08008281494205
    ## 53       D TRUE   home, away, 1.00607988285389, 1.2945708653023
    ## 54       A TRUE  home, away, 1.53726566520682, 1.34165983023155
    ## 55       D TRUE  home, away, 1.51025830001539, 1.03618222947824
    ## 56       D TRUE home, away, 1.29977957805942, 0.956397598648624
    ## 57       D TRUE    home, away, 1.398012554107, 1.09814131754795
    ## 58       A TRUE  home, away, 1.17681196459761, 1.74193547661846
    ## 59       H TRUE  home, away, 1.56037363361404, 1.16960792592318
    ## 60       D TRUE home, away, 1.52936248561464, 0.917203347104945
    ## 61       D TRUE   home, away, 2.1037854299245, 1.38835206589867
    ## 62       H TRUE  home, away, 1.49941472700497, 1.25636711736606
    ## 63       A TRUE home, away, 0.896822857713166, 1.63282848443797
    ## 64       H TRUE  home, away, 1.20130448747602, 1.21891917483428
    ## 65       D TRUE  home, away, 1.16429252265885, 1.19465094865309
    ## 66       H TRUE  home, away, 1.16954272658496, 1.41631877181155
    ## 67       A TRUE home, away, 0.887216487219788, 1.43941899852498
    ## 68       D TRUE  home, away, 1.02665945765451, 1.88973321040063
    ## 69       D TRUE   home, away, 1.5252202440912, 1.37819147289931
    ## 70       A TRUE  home, away, 1.27969629213881, 1.09951716187489
    ## 71       D TRUE    home, away, 1.4843408971561, 1.1136315353002
    ## 72       A TRUE  home, away, 1.24341410185346, 1.33478185222201
    ## 73       H TRUE  home, away, 1.89990659687099, 1.38333345917695
    ## 74       D TRUE  home, away, 1.86652819491182, 1.06836734285653
    ## 75       H TRUE home, away, 1.60639754775312, 0.986104501808628
    ## 76       H TRUE  home, away, 1.72780368037363, 1.13225095753711
    ## 77       A TRUE  home, away, 1.44131110156813, 2.32401330328353
    ## 78       D TRUE  home, away, 1.91108172620517, 1.56043918730447
    ## 79       A TRUE  home, away, 1.87310054210055, 1.22369215685649
    ## 80       A TRUE   home, away, 1.9312813828845, 1.51236110580675
    ## 81       H TRUE  home, away, 1.83642175377266, 1.67618946497077
    ## 82       A TRUE   home, away, 1.09839190953842, 2.1784475778521
    ## 83       D TRUE  home, away, 1.47130854057456, 1.62622807558946
    ## 84       D TRUE   home, away, 1.4259778017763, 1.59385048109804
    ## 85       H TRUE  home, away, 1.43240803653924, 1.88958989099296
    ## 86       A TRUE  home, away, 1.08662642035714, 1.92040919223085
    ## 87       A TRUE  home, away, 1.25741046009276, 2.52119850567215
    ## 88       D TRUE  home, away, 1.86802729431525, 1.83872213436266
    ## 89       D TRUE   home, away, 1.5673196126332, 1.46692718856973
    ## 90       A TRUE  home, away, 1.81795994427555, 1.48575796160825
    ## 91       H TRUE  home, away, 1.52288267179587, 1.78080693756077
    ## 92       H TRUE  home, away, 2.32692779508662, 1.84558234513105
    ## 93       A TRUE  home, away, 2.28604729527559, 1.42536847714486
    ## 94       A TRUE  home, away, 1.96744993147657, 1.31561703139536
    ## 95       H TRUE   home, away, 2.1161431908999, 1.51059917160647
    ## 96       H TRUE  home, away, 1.30429155426993, 1.65637603680137
    ## 97       H TRUE  home, away, 1.72940300834225, 1.11215975962147
    ## 98       D TRUE home, away, 1.69503253995764, 0.872152651697441
    ## 99       H TRUE  home, away, 1.74768236633605, 1.07789344024383
    ## 100      D TRUE  home, away, 2.33168054947919, 1.32015974383988
    ## 101      A TRUE home, away, 0.993972286295935, 1.55262810255178
    ## 102      H TRUE  home, away, 1.33143725952624, 1.15904896541436
    ## 103      D TRUE   home, away, 1.2904159285318, 1.13597273277443
    ## 104      A TRUE  home, away, 1.29623486719407, 1.34675279629455
    ## 105      A TRUE  home, away, 0.98332529401626, 1.36871839862965
    ## 106      D TRUE  home, away, 1.13787359409452, 1.79691432183909
    ## 107      H TRUE  home, away, 1.69044158507504, 1.31049821332412
    ## 108      H TRUE  home, away, 1.41832095192702, 1.04551167562007
    ## 109      A TRUE  home, away, 1.64513393308346, 1.05893278692411
    ## 110      H TRUE   home, away, 1.37810844917958, 1.2692207627976
    ## 111      H TRUE  home, away, 2.10571629346738, 1.31538763831498
    ## 112      H TRUE  home, away, 2.06872213545399, 1.01589185647908
    ## 113      D TRUE home, away, 1.78041251904736, 0.937669556939346
    ## 114      H TRUE  home, away, 1.91497011888248, 1.07663767050118
    ## 115      H TRUE  home, away, 1.6951131340405, 0.990703815307322
    ## 116      D TRUE home, away, 2.24760617661978, 0.665199744869527
    ## 117      H TRUE home, away, 2.20293684468158, 0.521647826562145
    ## 118      H TRUE  home, away, 2.2713628127152, 0.644704535696178
    ## 119      H TRUE home, away, 3.03035184953055, 0.789607713453145
    ## 120      H TRUE home, away, 2.15979924880263, 0.714542939912717
    ## 121      D TRUE home, away, 1.73039285456168, 0.693244895272359
    ## 122      H TRUE home, away, 1.67707977688602, 0.679442647949675
    ## 123      A TRUE home, away, 1.68464231865078, 0.805513424440358
    ## 124      H TRUE home, away, 1.27797164327589, 0.818651386808452
    ## 125      H TRUE  home, away, 1.47882922948707, 1.07476191086664
    ##  [ reached getOption("max.print") -- omitted 255 rows ]

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
```

    ## # A tibble: 380 x 6
    ##    home                  away               home_win draw  away_win result
    ##    <fct>                 <fct>              <chr>    <chr> <chr>    <fct> 
    ##  1 Aston Villa           West Ham United    56%      24%   20%      H     
    ##  2 Blackburn Rovers      Everton            34%      30%   36%      H     
    ##  3 Bolton Wanderers      Fulham             38%      30%   30%      D     
    ##  4 Chelsea               West Bromwich Alb… 78%      16%   8%       H     
    ##  5 Sunderland            Birmingham City    50%      30%   20%      D     
    ##  6 Tottenham Hotspur     Manchester City    32%      32%   36%      D     
    ##  7 Wigan Athletic        Blackpool          46%      26%   28%      A     
    ##  8 Wolverhampton Wander… Stoke City         36%      30%   34%      H     
    ##  9 Liverpool             Arsenal            40%      28%   32%      D     
    ## 10 Manchester United     Newcastle United   72%      18%   10%      H     
    ## # ... with 370 more rows

Or to get model parameters in a table format:

``` r
tidy(fit)
```

    ## # A tibble: 42 x 3
    ##    parameter team                value
    ##    <chr>     <chr>               <dbl>
    ##  1 off       Arsenal           0.369  
    ##  2 off       Aston Villa      -0.0295 
    ##  3 off       Birmingham City  -0.273  
    ##  4 off       Blackburn Rovers -0.0608 
    ##  5 off       Blackpool         0.142  
    ##  6 off       Bolton Wanderers  0.0420 
    ##  7 off       Chelsea           0.304  
    ##  8 off       Everton           0.0118 
    ##  9 off       Fulham           -0.00836
    ## 10 off       Liverpool         0.162  
    ## # ... with 32 more rows

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
