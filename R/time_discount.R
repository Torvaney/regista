#' Time discounting
#'
#' @description
#' Functions for downweighting games according to when they were played.
#'
#' @param days_ago The number of days ago as an integer or double.
#' @param rate,threshold Parameters controlling the discount rate, and which
#' games to discount, repectively.
#'
#' @export
discount_exponential <- function(days_ago, rate) {
  exp(-rate * days_ago)
}

#' @rdname discount_exponential
#' @export
discount_lte <- function(days_ago, threshold) {
  ifelse(days_ago <= threshold, 1, 0)
}
