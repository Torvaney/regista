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
#' @param f1 A formula describing the model for home goals
#' @param f2 A formula describing the model for away goals
#' @param data Data frame, list or environment (or object coercible by
#' `as.data.frame` to a data frame) containing the variables in the model.
#'
#' @return A list with component `par` containing the best set of parameters
#' found. See `optim` for details.
#'
#' @importFrom stats optim
#' @export
#' @examples
#' # Add home field advantage dummy
#' games <- premier_league_2010
#' games$hfa <- TRUE
#'
#' fit <- dixoncoles(hgoal ~ off(home) + def(away) + hfa + 0,
#'                   agoal ~ off(home) + def(home) + 0,
#'                   data = games)
dixoncoles <- function(f1, f2, data) {
  modeldata <- dc_modeldata(f1, f2, data)

  params <- rep_len(0, length(modeldata$vars) + 1)
  names(params) <- c(modeldata$vars, "rho")

  res <- optim(
    params,
    dc_objective_function,
    method = "BFGS",
    modeldata = modeldata
  )

  res
}

# Auxiliary functions ---------------------------------------------------------

#' Get model data for a Dixon-Coles model
#' @keywords internal
#' @importFrom purrr %>% map reduce flatten_chr
#' @importFrom lazyeval f_eval_lhs
dc_modeldata <- function(f1, f2, data) {
  terms1 <- quo_terms(f1)
  terms2 <- quo_terms(f2)

  # Create the model matrices
  mat1 <-
    map(terms1, term_matrix, data = data) %>%
    reduce(cbind)
  mat2 <-
    map(terms2, term_matrix, data = data) %>%
    reduce(cbind)

  column_names <- unique(c(colnames(mat1), colnames(mat2)))

  # Fill in missing parameters
  mat1 <- reduce(column_names, fill_if_missing, .init = mat1)
  mat2 <- reduce(column_names, fill_if_missing, .init = mat2)

  # Ensure both matrices have the same column ordering
  mat1 <- mat1[, column_names]
  mat2 <- mat2[, column_names]

  list(
    vars = column_names,
    y1 = f_eval_lhs(f1, data),
    y2 = f_eval_lhs(f2, data),
    mat1 = mat1,
    mat2 = mat2
  )
}

#' Function controlling dependence between home and away goals
#' @keywords internal
tau <- function(hg, ag, home_rates, away_rates, rho) {
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
dc_negloglike <- function(hg, ag, home_rates, away_rates, rho) {
  hprob <- dpois(hg, home_rates, log = TRUE)
  aprob <- dpois(ag, away_rates, log = TRUE)

  -sum(hprob + aprob + log(tau(hg, ag, home_rates, away_rates, rho)))
}

#' Dixon-Coles objective function
#' @keywords internal
dc_objective_function <- function(params, modeldata) {
  rho <- params["rho"]
  rate_params <- matrix(params[names(params) != "rho"], nrow = 1)

  home_rates <- exp(rate_params %*% t(modeldata$mat1))
  away_rates <- exp(rate_params %*% t(modeldata$mat2))

  dc_negloglike(
    modeldata$y1,
    modeldata$y2,
    home_rates,
    away_rates,
    rho
  )
}

#' Quote terms of a formula
#' @keywords internal
#' @importFrom rlang parse_quo caller_env
#' @importFrom purrr %>% map
#' @importFrom stats terms
quo_terms <- function(f) {
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
make_dummies <- function(values) {
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
term_matrix <- function(expr, data) {
  values <- eval_tidy(expr, data)

  if (is.factor(values)) {
    return(make_dummies(values))
  }

  matrix(values, dimnames = list(NULL, quo_name(expr)))
}

#' Add column to a matrix, if it doesn't exist
#' @keywords internal
fill_if_missing <- function(mat, name) {
  if (!(name %in% colnames(mat))) {
    blank_column <- matrix(0, nrow = nrow(mat), ncol = 1,
                           dimnames = list(NULL, name))
    return(cbind(mat, blank_column))
  }
  mat
}
