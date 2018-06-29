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
scorelines_to_outcomes <- function(scorelines, hgoal = ~hgoal, agoal = ~agoal, prob = ~prob) {
  require(dplyr)
  scorelines %>%
    mutate(outcome = case_when(
      hgoal > agoal  ~ "home_win",
      agoal > hgoal  ~ "away_win",
      hgoal == agoal ~ "draw"
    )) %>%
    count(outcome, wt = prob) %>%
    rename(prob = n)
}
