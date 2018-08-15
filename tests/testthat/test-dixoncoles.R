context("Dixon-Coles")

test_that("Tau(rho=0) is always 1", {
  expect_tau_0 <- function(hg, ag, hr, ar) {
    expect_equal(regista:::.tau(hg, ag, hr, ar, 0), 1)
  }

  expect_tau_0(0, 0, 0.8, 1.2)
  expect_tau_0(1, 0, 1.2, 0.8)
  expect_tau_0(0, 1, 0.8, 0.8)
  expect_tau_0(1, 1, 1.2, 1.2)
  expect_tau_0(5, 3, 1.0, 1.0)
})

test_that("Dependence calculation (tau) is correct", {
  expect_equal(regista:::.tau(0, 0, 1.2, 1.2, -0.2), 1.288)
  expect_equal(regista:::.tau(0, 0, 1.2, 1.2,  0.2), 0.712)
  expect_equal(regista:::.tau(1, 0, 1.0, 0.8, -0.2), 0.84)
  expect_equal(regista:::.tau(1, 0, 1.0, 0.8,  0.2), 1.16)
  expect_equal(regista:::.tau(0, 1, 0.8, 1.0, -0.2), 0.84)
  expect_equal(regista:::.tau(0, 1, 0.8, 1.0,  0.2), 1.16)
  expect_equal(regista:::.tau(1, 1, 0.8, 0.8, -0.2), 1.2)
  expect_equal(regista:::.tau(1, 1, 0.8, 0.8,  0.2), 0.8)
})

test_that("Negative log likelihood follows poisson likelihood", {
  expect_nll_pois <- function(hg, ag, hr, ar) {
    dc_nll <- regista:::.dc_negloglike(hg, ag, hr, ar, rho = 0, weights = 1)
    pois_nll <- -(dpois(hg, hr, log = TRUE) + dpois(ag, ar, log = TRUE))
    expect_equal(dc_nll, pois_nll)
  }

  expect_nll_pois(0, 0, 1.0, 1.0)
  expect_nll_pois(3, 1, 1.2, 1.2)
  expect_nll_pois(2, 2, 0.7, 1.8)
  expect_nll_pois(0, 1, 1.4, 1.1)
})

test_that("Negative log likelihood is using dependence parameter", {
  expect_nll_dep <- function(hg, ag, hr, ar, rho) {
    dc_nll <- regista:::.dc_negloglike(hg, ag, hr, ar, rho, weights = 1)
    pois_ll <- dpois(hg, hr, log = TRUE) + dpois(ag, ar, log = TRUE)
    exp_nll <- -(pois_ll + log(regista:::.tau(hg, ag, hr, ar, rho)))
    expect_equal(dc_nll, exp_nll)
  }

  expect_nll_dep(0, 0, 1.0, 1.0, -0.1)
  expect_nll_dep(3, 1, 1.2, 1.2,  0.1)
  expect_nll_dep(2, 2, 0.7, 1.8, -0.2)
  expect_nll_dep(0, 1, 1.4, 1.1,  0.2)
  expect_nll_dep(0, 0, 1.0, 1.0,  0.0)
  expect_nll_dep(3, 1, 1.2, 1.2,  0.11)
  expect_nll_dep(2, 2, 0.7, 1.8, -0.44)
  expect_nll_dep(0, 1, 1.4, 1.1, -0.21)
})

test_that("Constructing dummy variables from factors works", {
  expect_equal_mat <- function(m1, m2) {
    expect_equal(colnames(m1), colnames(m2))
    expect_equal(dim(m1), dim(m2))
    expect_equal(as.double(m1), as.double(m2))
  }

  fct <- factor(c("a", "b", "a"))
  mat <- matrix(
    c(1, 0, 1,  0, 1, 0),
    nrow = 3, ncol = 2,
    dimnames = list(NULL, c("a", "b"))
  )

  expect_equal_mat(regista:::.make_dummies(fct), mat)
})

test_that("Missing columns are filled in", {
  mat <- matrix(
    c(1, 0, 1,
      0, 1, 0),
    nrow = 3, ncol = 2,
    dimnames = list(NULL, c("a", "b"))
  )

  expect_cols <- function(m, col, cols) {
    # Check that column is added...
    filled <- regista:::.fill_if_missing(m, col)
    expect_equal(colnames(filled), cols)
    # And that values are all 0
    if (!(col %in% colnames(m))) {
      zeroes <- rep(0, nrow(m))
      expect_equal(filled[, col], zeroes)
    }
  }

  expect_cols(mat, "a", c("a", "b"))
  expect_cols(mat, "b", c("a", "b"))
  expect_cols(mat, "c", c("a", "b", "c"))
  expect_cols(mat, "longname", c("a", "b", "longname"))
})

test_that("Rates are calculated correctly", {
  rho <- -0.14
  params <- c(off___teamA =  0.0, off___teamB = 0.0,
              def___teamA = -0.1, def___teamB = 0.1,
              rho = rho)
  modeldata <- list(
    mat1 = matrix(c(1, 0, 0, 1), nrow = 1),
    mat2 = matrix(c(0, 1, 1, 0), nrow = 1)
  )

  rates <- regista:::.dc_rate_info(params, modeldata)

  expect_equivalent(rates$rho, rho)
  expect_equivalent(rates$home, exp(params["off___teamA"] + params["def___teamB"]))
  expect_equivalent(rates$away, exp(params["off___teamB"] + params["def___teamA"]))
})

# Handled by an error/warning for now
# test_that("Simple Dixon-Coles can handle character teams", {
#   suppressWarnings({
#     fit_fct <- dixoncoles(hgoal, agoal, home, away, premier_league_2010)
#
#     premier_league_2010$home <- as.character(premier_league_2010$home)
#     premier_league_2010$away <- as.character(premier_league_2010$away)
#     fit_chr <- dixoncoles(~hgoal, ~agoal, ~home, ~away, premier_league_2010)
#   })
#
#   expect_equal(fit_chr$par, fit_fct$par)
# })

test_that("Both Dixon-Coles function return the same estimates", {
  seed <- 2018-06-03
  set.seed(seed)
  fit_simple <- suppressWarnings(
    dixoncoles(hgoal, agoal, home, away, premier_league_2010)
  )
  pars_simple <- sort(fit_simple$par)

  set.seed(seed)
  fit_ext <- suppressWarnings(
    dixoncoles_ext(f1 = hgoal ~ off(home) + def(away) + hfa + 0,
                   f2 = agoal ~ off(away) + def(home) + 0,
                   weights = 1,
                   data = premier_league_2010)
  )
  pars_ext <- sort(fit_ext$par)

  expect_equal(pars_simple, pars_ext)
})

test_that("Home advantage estimates are reasonable", {
  skip_if_not_installed("rsample")
  set.seed(2018)
  resampled_data <- rsample::bootstraps(premier_league_2010, times = 5)

  lapply(resampled_data$splits, function(data) {
    # Supress warnings related to poorly specified bounds (see #1)
    fit <- suppressWarnings(
      dixoncoles(
        hgoal, agoal, home, away,
        as.data.frame(data)
      )
    )

    hfa <- fit$par[["hfa"]]
    expect_gt(hfa, 0.1)
    expect_lt(hfa, 0.5)
  })
})

test_that("Weighting games works", {
  # Give games where the home team wins a higher weight and compare HFA
  # TODO: Come up with better + more granular tests for this feature
  seed <- 2018-06-17
  suppressWarnings({
    set.seed(seed)
    equal_weight <- dixoncoles(
      hgoal   = hgoal,
      agoal   = agoal,
      hteam   = home,
      ateam   = away,
      weights = 1,
      data    = premier_league_2010
    )

    set.seed(seed)
    high_hfa <- dixoncoles(
      hgoal   = hgoal,
      agoal   = agoal,
      hteam   = home,
      ateam   = away,
      weights = ifelse(hgoal > agoal, 1, 0.5),
      data    = premier_league_2010
    )
  })
  expect_gt(high_hfa$par[["hfa"]], equal_weight$par[["hfa"]])

  suppressWarnings({
    set.seed(seed)
    equal_weight <- dixoncoles_ext(
      f1      = hgoal ~ off(home) + def(away) + hfa + 0,
      f2      = agoal ~ off(away) + def(home) + 0,
      weights = 1,
      data    = premier_league_2010
    )

    set.seed(seed)
    high_hfa <- dixoncoles_ext(
      f1      = hgoal ~ off(home) + def(away) + hfa + 0,
      f2      = agoal ~ off(away) + def(home) + 0,
      weights = ifelse(hgoal > agoal, 1, 0.5),
      data    = premier_league_2010
    )
  })
  expect_gt(high_hfa$par[["hfa"]], equal_weight$par[["hfa"]])
})

test_that("Games can be predicted", {
  suppressWarnings({
    fit_simple <- dixoncoles(hgoal, agoal, home, away, premier_league_2010)
    fit_ext <- dixoncoles_ext(
      f1      = hgoal ~ off(home) + def(away) + hfa + 0,
      f2      = agoal ~ off(away) + def(home) + 0,
      weights = 1,
      data    = premier_league_2010
    )
  })

  rates_simple <- predict(fit_simple, premier_league_2010, type = "rates")
  rates_ext <- predict(fit_ext, premier_league_2010, type = "rates")
  expect_equal(rates_simple, rates_ext)

  scorelines_simple <- predict(fit_simple, premier_league_2010, type = "scorelines")
  scorelines_ext <- predict(fit_ext, premier_league_2010, type = "scorelines")
  expect_equal(scorelines_simple, scorelines_ext)

  # A single game can also be predicted
  first_game <- head(premier_league_2010, 1)
  rates_simple <- predict(fit_simple, first_game, type = "rates")
  rates_ext <- predict(fit_ext, first_game, type = "rates")
  expect_equal(rates_simple, rates_ext)
})
