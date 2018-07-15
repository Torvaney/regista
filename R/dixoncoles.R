#' Dixon-Coles model for estimating team strengths
#'
#' @description
#'
#' This is an implementation of the Dixon-Coles model for estimating soccer
#' teams' strength from goals scored and conceded:
#'
#' Dixon, Mark J., and Stuart G. Coles. "Modelling association football scores
#' and inefficiencies in the football betting market." Journal of the Royal
#' Statistical Society: Series C (Applied Statistics) 46, no. 2 (1997):
#' 265-280.
#'
#' @param hgoal A formula describing the home goals column in `data`, or a
#'   numeric vector containing the observed home goals for a set of games.
#' @param agoal A formula describing the away goals column in `data`, or a
#'   numeric vector containing the observed away goals for a set of games.
#' @param hteam A formula describing the home team column in `data`, or a
#'   vector containing the home team name for a set of games.
#' @param ateam A formula describing the away team column in `data`, or a
#'   vector containing the away team name for a set of games.
#' @param data Data frame, list or environment (or object coercible by
#' `as.data.frame` to a data frame) containing the variables in the model.
#' @param weights A formula describing an expression to calculate the weight for
#'   each game. All games weighted equally by default.
#' @param ... Arguments passed onto `dixoncoles_ext`.
#'
#' @return A list with component `par` containing the best set of parameters
#'   found. See `optim` for details.
#'
#' @importFrom lazyeval f_eval f_interp f_new uq
#' @importFrom rlang quo_text
#' @export
#' @examples
#' fit <- dixoncoles(~hgoal, ~agoal, ~home, ~away, premier_league_2010)
#'
dixoncoles <- function(hgoal, agoal, hteam, ateam, data, weights = ~1, ...) {

  # Check input
  hvar <- f_eval(hteam, data)
  avar <- f_eval(ateam, data)
  if (!(is.factor(hvar) & is.factor(avar))) {
    stop("home and away team variables should be factors (see factor_teams)")
  }
  if (!setequal(levels(hvar), levels(avar))) {
    warning("home and away team variables should have the same levels (see factor_teams)")
  }

  # Fit the model
  f1 <- f_new(uq(f_interp(~ off(uq(hteam)) + def(uq(ateam)) + hfa + 0)), uq(hgoal))
  f2 <- f_new(uq(f_interp(~ off(uq(ateam)) + def(uq(hteam)) + 0)), uq(agoal))

  data$hfa <- TRUE

  res <- dixoncoles_ext(f1, f2, weights = weights, data = data, ...)

  # Hack to let predict.dixoncoles know to add HFA
  res$implicit_hfa <- TRUE

  res
}

#' A generic Dixon-Coles model for estimating team strengths
#'
#' @description
#'
#' This is an implementation of the Dixon-Coles model for estimating soccer
#' teams' strength from goals scored and conceded:
#'
#' Dixon, Mark J., and Stuart G. Coles. "Modelling association football scores
#' and inefficiencies in the football betting market." Journal of the Royal
#' Statistical Society: Series C (Applied Statistics) 46, no. 2 (1997):
#' 265-280.
#'
#' By specifying the model as a pair of formulas, it allows the user to
#' estimate the effect of parameters beyond team strength.
#'
#' @param f1 A formula describing the model for home goals.
#' @param f2 A formula describing the model for away goals.
#' @param weights A formula describing an expression to calculate the weight for
#'   each game.
#' @param data Data frame, list or environment (or object coercible by
#'   `as.data.frame` to a data frame) containing the variables in the model.
#' @param init Initial parameter values. If it is `NULL`, 0 is used for all
#'   values.
#' @param ... Arguments passed onto `optim`.
#'
#' @return A list with component `par` containing the best set of parameters
#'   found. See `optim` for details.
#'
#' @importFrom stats optim
#' @export
#' @examples
#' fit <- dixoncoles_ext(hgoal ~ off(home) + def(away) + hfa + 0,
#'                       agoal ~ off(away) + def(home) + 0,
#'                       weights = ~1,  # All games weighted equally
#'                       data = premier_league_2010)
dixoncoles_ext <- function(f1, f2, weights, data, init = NULL, ...) {
  # Handle args to pass onto optim including defaults
  dots <- list(...)

  if (!("method" %in% names(dots))) {
    dots["method"] <- "BFGS"
  }

  # Wrangle data and intial params
  modeldata <- .dc_modeldata(f1, f2, weights, data)

  if (is.null(init)) {
    params <- rep_len(0, length(modeldata$vars) + 1)
    names(params) <- c(modeldata$vars, "rho")
  } else {
    params <- init
  }

  # Create arguments to optim
  # We need to do this + do.call so that we can pass on ... with default args
  # Maybe there's a better way using rlang::list2?
  args <- c(
    list(par       = params,
         fn        = .dc_objective_function,
         modeldata = modeldata),
    dots
  )

  res <- do.call(optim, args)

  res$f1 <- f1
  res$f2 <- f2
  res$weights <- weights

  res$implicit_hfa <- FALSE

  structure(res, class = "dixoncoles")
}

# Dixon-Coles class ------------------------------------------------------------

#' @importFrom glue glue
#' @export
print.dixoncoles <- function(x, ...) {
  msg <- glue("Dixon-Coles model with specification:

               Home goals: {deparse(x$f1)}
               Away goals: {deparse(x$f2)}
               Weights   : {deparse(x$weights)}")

  cat("\n")
  cat(msg)
  cat("\n\n")
  invisible(x)
}

#' Predict method for Dixon-Coles model fits
#'
#' @description
#'
#' Predicted rates or scorelines based on a Dixon Coles model object
#'
#' @param object Object of class inheriting from `dixoncoles`.
#' @param newdata A data frame in which to look for variables to predict
#' @param type Type of prediction (rates or scorelines).
#' @param up_to If `type = "scorelines"`, the maximum number of goals for which
#'   to calculate the probability of occurring in each match.
#' @param threshold If `type = "scorelines"`, scorelines with a probability
#'   below `threshold` will not be returned.
#' @param ... Arguments passed from other methods
#'
#' @return If `type = "rates"`, a dataframe of each teams' estimated goalscoring
#' rate. If `type = "scorelines"`, a list in which each element is a dataframe
#' of scorelines and their estimated probabilities.
#'
#' @export
predict.dixoncoles <- function(object, newdata, type = c("rates", "scorelines"),
                               up_to = 50, threshold = 1e-8, ...) {

  if (object$implicit_hfa == TRUE) {
    newdata$hfa <- TRUE
  }

  # Create model matrix for newdata
  modeldata <- .dc_modeldata(
    object$f1,
    object$f2,
    object$weights,
    newdata
  )

  # Matrix multiplication to get Poisson means
  rate_info <- .dc_rate_info(object$par, modeldata)

  # Return only the rates if "response" is chosen
  rates <- data.frame(home_rate = c(rate_info$home),
                      away_rate = c(rate_info$away))
  if (type == "rates") {
    return(rates)
  }

  if (type == "scorelines") {
    return(.dc_predict_scorelines(rate_info, up_to, threshold))
  }

  warning("Unknown response type. Defaulting to type = \"rates\"")
  rates
}

#' Calculate the probability of scorelines occuring for a given set of matches
#' @keywords internal
#' @importFrom purrr map2
.dc_predict_scorelines <- function(rates, up_to, threshold) {
  # Calculate the probability of each scoreline for each game
  map2(
    rates$home,
    rates$away,
    .dc_predict_scorelines_once,
    rho       = rates$rho,
    up_to     = up_to,
    threshold = threshold
  )
}

#' Calculate the probability of scorelines occuring for a given match
#' @keywords internal
#' @importFrom stats dpois
#' @importFrom purrr map2_dbl
.dc_predict_scorelines_once <- function(home_rate, away_rate, rho, up_to, threshold) {
  home_probs <- dpois(0:up_to, home_rate)
  away_probs <- dpois(0:up_to, away_rate)

  scorelines <- expand.grid(hgoal = 0:up_to,
                            agoal = 0:up_to)

  hprob <- dpois(scorelines$hgoal, home_rate)
  aprob <- dpois(scorelines$agoal, away_rate)

  tau <- .tau(
    scorelines$hgoal,
    scorelines$agoal,
    home_rates = home_rate,
    away_rates = away_rate,
    rho = rho
  )

  scorelines$prob <- hprob * aprob * tau

  # Filter out the ~0% (< threshold) rows
  scorelines <- scorelines[scorelines$prob > threshold, ]

  scorelines
}

# Auxiliary fitting functions --------------------------------------------------

#' Get model data for a Dixon-Coles model
#' @keywords internal
#' @importFrom purrr %>% map reduce flatten_chr
#' @importFrom lazyeval f_eval_lhs
.dc_modeldata <- function(f1, f2, weights, data) {
  terms1 <- .quo_terms(f1)
  terms2 <- .quo_terms(f2)

  # Create the model matrices
  mat1 <-
    map(terms1, .term_matrix, data = data) %>%
    reduce(cbind)
  mat2 <-
    map(terms2, .term_matrix, data = data) %>%
    reduce(cbind)

  column_names <- unique(c(colnames(mat1), colnames(mat2)))

  # Fill in missing parameters
  mat1 <- reduce(column_names, .fill_if_missing, .init = mat1)
  mat2 <- reduce(column_names, .fill_if_missing, .init = mat2)

  # Ensure both matrices have the same column ordering
  # We have to use matrix to ensure that it retains it's dimensions when
  # there's just 1 observation (for instance when calling predict.dixoncoles)
  mat1 <- matrix(mat1[, column_names], nrow = nrow(mat1))
  mat2 <- matrix(mat2[, column_names], nrow = nrow(mat2))

  list(
    vars    = column_names,
    y1      = f_eval_lhs(f1, data),
    y2      = f_eval_lhs(f2, data),
    mat1    = mat1,
    mat2    = mat2,
    weights = f_eval(weights, data)
  )
}

#' Function controlling dependence between home and away goals
#' @keywords internal
.tau <- function(hg, ag, home_rates, away_rates, rho) {
  if (!all.equal(length(hg), length(ag),
                 length(home_rates), length(away_rates))) {
    stop("Supplied vectors must all have the same length")
  }

  # Initialise values to 1
  vals <- rep_len(1, length.out = length(hg))

  vals <- ifelse((hg == 0) & (ag == 0), 1 - home_rates * away_rates * rho, vals)
  vals <- ifelse((hg == 0) & (ag == 1), 1 + home_rates * rho, vals)
  vals <- ifelse((hg == 1) & (ag == 0), 1 + away_rates * rho, vals)
  vals <- ifelse((hg == 1) & (ag == 1), 1 - rho, vals)

  vals
}

#' Dixon-Coles negative log likelihood
#' @keywords internal
#' @importFrom stats dpois
.dc_negloglike <- function(hg, ag, home_rates, away_rates, rho, weights) {
  hprob <- dpois(hg, home_rates, log = TRUE)
  aprob <- dpois(ag, away_rates, log = TRUE)

  loglike <- hprob + aprob + log(.tau(hg, ag, home_rates, away_rates, rho))

  # Create weighted pseudo-log likelihood
  ploglike <- loglike * weights

  -sum(ploglike)
}

#' Get estimated rates for home and away goals
#' @keywords internal
.dc_rate_info <- function(params, modeldata) {
  rho <- params["rho"]
  rate_params <- matrix(params[names(params) != "rho"], nrow = 1)

  home_rates <- exp(rate_params %*% t(modeldata$mat1))
  away_rates <- exp(rate_params %*% t(modeldata$mat2))

  list(home = home_rates,
       away = away_rates,
       rho  = rho)
}

#' Dixon-Coles objective function
#' @keywords internal
.dc_objective_function <- function(params, modeldata) {
  rates <- .dc_rate_info(params, modeldata)

  .dc_negloglike(
    modeldata$y1,
    modeldata$y2,
    rates$home,
    rates$away,
    rates$rho,
    modeldata$weights
  )
}

#' Quote terms of a formula
#' @keywords internal
#' @importFrom rlang parse_quo caller_env
#' @importFrom purrr %>% map
#' @importFrom stats terms
.quo_terms <- function(f) {
  t <- terms(f)

  if (attr(t, "intercept")) {
    warning("Intercept term will be ignored")
  }

  t %>%
    attr("term.labels") %>%
    map(parse_quo, env = caller_env())
}

#' Get a matrix of dummy variables from a factor
#' @keywords internal
#' @importFrom stats model.frame model.matrix
.make_dummies <- function(values) {
  mat <- model.matrix(
    ~ values - 1,
    model.frame(~ values - 1),
    contrasts = FALSE
  )
  colnames(mat) <- gsub("^values", "", colnames(mat))

  mat
}

#' Get a model matrix from an expression
#' @keywords internal
#' @importFrom rlang eval_tidy quo_name
.term_matrix <- function(expr, data) {
  values <- eval_tidy(expr, data)

  if (is.factor(values)) {
    return(.make_dummies(values))
  }

  matrix(values, dimnames = list(NULL, quo_name(expr)))
}

#' Add column to a matrix, if it doesn't exist
#' @keywords internal
.fill_if_missing <- function(mat, name) {
  if (!(name %in% colnames(mat))) {
    blank_column <- matrix(0, nrow = nrow(mat), ncol = 1,
                           dimnames = list(NULL, name))
    return(cbind(mat, blank_column))
  }
  mat
}
