# Internal package utilities

#' @keywords internal
log_quietly <- purrr::compose(
  function(x) purrr::pluck(x, "result"),
  purrr::quietly(log)
)
