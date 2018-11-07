#' Aggregate scoreline probabilities to 1X2 probabilities
#'
#' @description
#' Often we are more interested in the outcome of a match (home win, draw,
#' away win) than we are in the scorelines. This is a helper function that
#' aggregates scoreline probabilities to outcome probabilities.
#'
#' @param scorelines A dataframe containing the scoreline probabilities
#' @param hgoal The column containing home goals
#' @param agoal The column containing away goals
#' @param prob The column containing the probability of that given scoreline
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom purrr map_dbl
#' @importFrom glue glue
#'
#' @export
scorelines_to_outcomes <- function(scorelines,
                                   hgoal = hgoal,
                                   agoal = agoal,
                                   prob  = prob) {
  hgoals <- eval_tidy(enquo(hgoal), scorelines)
  agoals <- eval_tidy(enquo(agoal), scorelines)

  # Determine the result for each scoreline
  scorelines$outcome <- NA_character_
  scorelines$outcome <- ifelse(hgoals > agoals, "home_win", scorelines$outcome)
  scorelines$outcome <- ifelse(agoals > hgoals, "away_win", scorelines$outcome)
  scorelines$outcome <- ifelse(agoals == hgoals, "draw", scorelines$outcome)

  sum_probs <- function(outcome) {
    sum(eval_tidy(enquo(prob), scorelines[scorelines$outcome == outcome, ]))
  }

  # NOTE: Should outcomes be a factor?
  #       There are 3 clear levels and not all of them are guaranteed to appear
  #       Tidyverse convention appears to be to favour characters
  outcomes <- c("home_win", "draw", "away_win")
  outcome_probs <- tibble::tibble(
    outcome = outcomes,
    prob    = map_dbl(outcomes, sum_probs)
  )

  # Check that (almost) all probability is assigned
  total_prob <- sum(outcome_probs$prob)
  tolerance <- 1e-5
  if (is.character(all.equal(total_prob, 1.0, tolerance = tolerance))) {
    warning(glue("Outcome probabilites expected to sum to 1. Provided values ",
                 "sum to {total_prob}."))
  }

  outcome_probs
}
