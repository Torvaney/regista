#' Coerce teamnames into factors
#'
#' @description
#' Modelling team strengths often requires separate parameters for offence and
#' defence. These helper functions provide a quick way to turn a factor of team
#' names into a factor of offence or defence levels
#'
#' @param .data A data frame containing match data.
#' @param columns A charater vector containing the names of columns
#' containing team names.
#'
#' @importFrom purrr reduce
#' @export
#' @examples
#' # For a given dataframe of matches...
#' games <- data.frame(
#'   home = c("Arsenal", "Barcelona", "Arsenal"),
#'   away = c("Chelsea", "Roma",      "Barcelona"),
#'   result = c("H", "A", "H"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Convert the teams to factors
#' games <- factor_teams(games, c("home", "away"))
#' levels(games$home)
#' #> c("Arsenal", "Barcelona", "Chelsea", "Roma")
factor_teams <- function(.data, columns) {
  column_vals <- reduce(columns, ~ c(.x, as.character(.data[[.y]])), .init = c())
  levels <- unique(column_vals)

  reduce(columns, ~ .factor_column(.x, .y, levels), .init = .data)
}

#' Coerce a single column to a factor
#' @keywords internal
.factor_column <- function(.data, column, levels) {
  .data[[column]] <- factor(.data[[column]], levels = levels)
  .data
}
