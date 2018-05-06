#' Dixon-Coles model
#'
#' This is a description
#'
#' @details
#' Some details
#'
#' @examples
#' mtcars
#'
#' @export
#'
#' @importFrom stats optim
dixoncoles <- function(f1, f2, data) {
  modeldata <- dc_modeldata(f1, f2, data)

  params <- rep_len(0, length(modeldata$vars) + 1)
  names(params) <- c(modeldata$vars, "rho")

  res <- optim(params, dc_objective_function, method = "BFGS", modeldata = modeldata)

  res
}

#' Quote terms of a formula
#' @importFrom rlang parse_quosure
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom stats terms
quo_terms <- function(f) {
  t <- terms(f)

  if (attr(t, "intercept")) {
    warning("Intercept term will be ignored")
  }

  t %>%
    attr("term.labels") %>%
    map(parse_quosure)
}

#' Get the name of variables from an expression
#' @importFrom rlang eval_tidy quo_name
var_name <- function(expr, data) {
  values <- eval_tidy(expr, data)

  if (is.factor(values)) {
    return(levels(values))
  }

  quo_name(expr)
}

#' Get a matrix of dummy variables from a factor
#' @importFrom stats model.frame model.matrix
#' @importFrom stringr str_replace_all
make_dummies <- function(values) {
  mat <- model.matrix(~ values - 1, model.frame(~ values - 1), contrasts = FALSE)
  colnames(mat) <- str_replace_all(colnames(mat), "^values", "")

  mat
}

#' Get a model matrix from an expression
#' @importFrom rlang eval_tidy quo_name
term_matrix <- function(expr, data) {
  values <- eval_tidy(expr, data)

  if (is.factor(values)) {
    return(make_dummies(values))
  }

  matrix(values, dimnames = list(NULL, quo_name(expr)))
}

#' Add column to a matrix, if it doesn't exist
fill_if_missing <- function(mat, name) {
  if (!(name %in% colnames(mat))) {
    blank_column <- matrix(0, nrow = nrow(mat), ncol = 1, dimnames = list(NULL, name))
    return(cbind(mat, blank_column))
  }
  mat
}

#' Get model data for a Dixon-Coles model
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce flatten_chr
#' @importFrom lazyeval f_eval_lhs
dc_modeldata <- function(f1, f2, data) {
  terms1 <- quo_terms(f1)
  terms2 <- quo_terms(f2)

  all_terms <- unique(c(terms1, terms2))

  column_names <-
    map(all_terms, var_name, data = data) %>%
    flatten_chr() %>%
    unique() %>%
    sort()

  # Create the model matrices
  mat1 <-
    map(terms1, term_matrix, data = data) %>%
    reduce(cbind)
  mat2 <-
    map(terms2, term_matrix, data = data) %>%
    reduce(cbind)

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
#' @importFrom dplyr case_when
tau <- function(hg, ag, home_rates, away_rates, rho) {
  case_when(
    (hg == 0) & (ag == 0) ~ 1 - home_rates * away_rates * rho,
    (hg == 0) & (ag == 1) ~ 1 + home_rates * rho,
    (hg == 1) & (ag == 0) ~ 1 + away_rates * rho,
    (hg == 1) & (ag == 1) ~ 1 - rho,
    TRUE ~ 1
  )
}

#' Dixon-Coles negative log likelihood
#' @importFrom stats dpois
negloglike <- function(hg, ag, home_rates, away_rates, rho) {
  hprob <- dpois(hg, home_rates, log = TRUE)
  aprob <- dpois(ag, away_rates, log = TRUE)

  -sum(hprob + aprob + log(tau(hg, ag, home_rates, away_rates, rho)))
}

#' Dixon-Coles objective function
dc_objective_function <- function(params, modeldata) {
  rho <- params["rho"]
  rate_params <- matrix(params[names(params) != "rho"], nrow = 1)

  home_rates <- exp(rate_params %*% t(modeldata$mat1))
  away_rates <- exp(rate_params %*% t(modeldata$mat2))

  negloglike(
    modeldata$y1,
    modeldata$y2,
    home_rates,
    away_rates,
    rho
  )
}
