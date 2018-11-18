context("Dixon-Coles")


# Test the internals -----------------------------------------------------------


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


test_that("Normalising offence parameters works", {
  expect_params_normalised <- function(off_a, off_b, def_a, def_b, rho) {
    params <- c(off___teamA = off_a, off___teamB = off_b,
                def___teamA = def_a, def___teamB = def_b,
                rho = rho)

    params_normalised <- regista:::.normalise_off_params(params)

    # Expect defence parameters unchanged
    expect_equivalent(params_normalised["def___teamA"], params["def___teamA"])
    expect_equivalent(params_normalised["def___teamB"], params["def___teamB"])
    expect_equivalent(params_normalised["rho"], params["rho"])

    # Expect mean(offence params) = 1
    exp_params <- exp(params_normalised)
    off_params <- exp_params[startsWith(names(exp_params), "off___")]
    expect_equal(mean(off_params), 1)
  }

  expect_params_normalised(1.0,  1.0, -0.1, 0.1, -0.10)
  expect_params_normalised(0.0,  0.0, -0.1, 0.1, -0.05)
  expect_params_normalised(-0.2, 0.2, -0.3, 0.3, -0.10)
  expect_params_normalised(0.1, -0.1,  0.0, 0.0,  0.10)
  expect_params_normalised(0.1, -0.1, -1.0, 1.0,  0.00)
})


# Extrinsic tests  -------------------------------------------------------------


test_that("Both Dixon-Coles functions return the same estimates", {
  seed <- 2018-06-03

  set.seed(seed)
  fit_simple <- dixoncoles(hgoal, agoal, home, away, premier_league_2010)
  pars_simple <- sort(fit_simple$par)

  set.seed(seed)
  fit_ext <- dixoncoles_ext(
    f1 = hgoal ~ off(home) + def(away) + hfa + 0,
    f2 = agoal ~ off(away) + def(home) + 0,
    weights = 1,
    data = premier_league_2010
  )
  pars_ext <- sort(fit_ext$par)

  expect_equal(pars_simple, pars_ext)

  # While we're here, check that parameters are normalised
  expect_equal(mean(exp(pars_simple[startsWith(names(pars_simple), "off___")])), 1)
  expect_equal(mean(exp(pars_ext[startsWith(names(pars_ext), "off___")])), 1)
})


test_that("Home advantage estimates are reasonable", {
  skip_if_not_installed("modelr")
  set.seed(2018)
  lapply(1:5, function(data) {
    resampled_data <- modelr::resample_bootstrap(premier_league_2010)
    fit <- dixoncoles(
      hgoal, agoal, home, away,
      as.data.frame(resampled_data)
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

  expect_gt(high_hfa$par[["hfa"]], equal_weight$par[["hfa"]])

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

  expect_gt(high_hfa$par[["hfa"]], equal_weight$par[["hfa"]])
})


test_that("Games can be predicted", {

  fit_simple <- dixoncoles(hgoal, agoal, home, away, premier_league_2010)
  fit_ext <- dixoncoles_ext(
    f1      = hgoal ~ off(home) + def(away) + hfa + 0,
    f2      = agoal ~ off(away) + def(home) + 0,
    weights = 1,
    data    = premier_league_2010
  )

  games_to_predict <- premier_league_2010[, c("home", "away", "hfa")]

  rates_simple <- predict(fit_simple, games_to_predict, type = "rates")
  rates_ext <- predict(fit_ext, games_to_predict, type = "rates")
  expect_equal(rates_simple, rates_ext)

  scorelines_simple <- predict(fit_simple, games_to_predict, type = "scorelines")
  scorelines_ext <- predict(fit_ext, games_to_predict, type = "scorelines")
  expect_equal(scorelines_simple, scorelines_ext)

  outcomes_simple <- predict(fit_simple, games_to_predict, type = "outcomes")
  outcomes_ext <- predict(fit_ext, games_to_predict, type = "outcomes")
  expect_equal(outcomes_simple, outcomes_ext)

  # A single game can also be predicted
  first_game <- head(games_to_predict, 1)
  rates_simple <- predict(fit_simple, first_game, type = "rates")
  rates_ext <- predict(fit_ext, first_game, type = "rates")
  expect_equal(rates_simple, rates_ext)


  # Inconsistent factor levels will raise an error
  newdata <- subset(premier_league_2010, (home != "Arsenal") & (away != "Arsenal"))
  expect_error(predict(fit_simple, newdata = factor_teams(newdata, c("home", "away"))),
               "New data must have the same factor levels as the data used to fit")
})
