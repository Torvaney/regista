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
#' @importFrom lazyeval f_eval
#' @importFrom purrr map_dbl
#' @importFrom glue glue
#'
#' @export
scorelines_to_outcomes <- function(scorelines, hgoal = ~hgoal, agoal = ~agoal, prob = ~prob) {
  hgoals <- f_eval(hgoal, scorelines)
  agoals <- f_eval(agoal, scorelines)

  # Determine the result for each scoreline
  scorelines$outcome <- NA_character_
  scorelines$outcome <- ifelse(hgoals > agoals, "home_win", scorelines$outcome)
  scorelines$outcome <- ifelse(agoals > hgoals, "away_win", scorelines$outcome)
  scorelines$outcome <- ifelse(agoals == hgoals, "draw", scorelines$outcome)

  sum_probs <- function(outcome) {
    sum(f_eval(prob, scorelines[scorelines$outcome == outcome, ]))
  }

  # NOTE: Should outcomes be a factor?
  #       There are 3 clear levels and not all of them are guaranteed to appear
  #       Tidyverse convention appears to be to favour characters
  outcomes <- c("home_win", "draw", "away_win")
  outcome_probs <- data.frame(
    outcome = outcomes,
    prob    = map_dbl(outcomes, sum_probs),
    stringsAsFactors = FALSE
  )

  if (sum(outcome_probs$prob) != 1) {
    warning(glue("Outcome probabilites expected to sum to 1. Provided values ",
                 "sum to {sum(outcome_probs$prob)}."))
  }

  outcome_probs
}
