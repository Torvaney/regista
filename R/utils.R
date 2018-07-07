#' Helper functions to enable easier model specification
#'
#' @description
#' Modelling team strengths often requires separate parameters for offence and
#' defence. These helper functions provide a quick way to turn a factor of team
#' names into a factor of offence or defence levels
#'
#' @param x A factor containing team names
#'
#' @examples
#' teams <- factor(c("Arsenal", "Liverpool", "Barcelona", "Dortmund"))
#' off(teams)
#' #> [1] off___Arsenal   off___Liverpool off___Barcelona off___Dortmund
#' #> Levels: off___Arsenal off___Barcelona off___Dortmund off___Liverpool
#'
#' @importFrom forcats fct_relabel
#' @export
off <- function(x) {
  fct_relabel(x, .off_str)
}

#' @rdname off
#' @importFrom forcats fct_relabel
#' @export
def <- function(x) {
  fct_relabel(x, .def_str)
}

#' @keywords internal
.off_str <- function(x) {
  paste0("off___", x)
}

#' @keywords internal
.def_str <- function(x) {
  paste0("def___", x)
}

# ------------------------------------------------------------------------------

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
