context("Utils")


test_that("Log warnings are be suppressed", {
  expect_warning(regista:::log_quietly(-1),  NA)
  expect_warning(regista:::log_quietly(-10), NA)
})


test_that("Valid log(x) values are returned", {
  expect_equal_log <- function(x) {
    expect_equal(regista:::log_quietly(x), log(x))
  }

  expect_equal_log(1)
  expect_equal_log(2)
  expect_equal_log(pi)
  expect_equal_log(exp(1))
  expect_equal_log(143)
})
