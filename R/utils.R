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
#' team_off <- off(teams)
#'
#' @export
#' @importFrom forcats fct_relabel
off <- function(x) {
  fct_relabel(x, off_str)
}

#' @rdname off
#' @export
#' @importFrom forcats fct_relabel
def <- function(x) {
  fct_relabel(x, def_str)
}

#' @keywords internal
off_str <- function(x) {
  paste0("off___", x)
}

#' @keywords internal
def_str <- function(x) {
  paste0("def___", x)
}
