context("Factor teams")

test_that("Character vectors are factorised correctly", {
  games <- data.frame(
    home = c("Arsenal", "Barcelona", "Arsenal"),
    away = c("Chelsea", "Roma",      "Barcelona"),
    result = c("H", "A", "H"),
    stringsAsFactors = FALSE
  )

  games_fct <- factor_teams(games, c("home", "away"))
  expect_equal(c("Arsenal", "Barcelona", "Chelsea", "Roma"), levels(games_fct$home))
  expect_equal(c("Arsenal", "Barcelona", "Chelsea", "Roma"), levels(games_fct$away))
})

test_that("Factors are factorised correctly", {
  games <- data.frame(
    home = c("Arsenal", "Barcelona", "Arsenal"),
    away = c("Chelsea", "Roma",      "Barcelona"),
    result = c("H", "A", "H"),
    stringsAsFactors = TRUE
  )

  games_fct <- factor_teams(games, c("home", "away"))
  expect_equal(c("Arsenal", "Barcelona", "Chelsea", "Roma"), levels(games_fct$home))
  expect_equal(c("Arsenal", "Barcelona", "Chelsea", "Roma"), levels(games_fct$away))
})
