context("Scorelines to outcomes")


test_that("Scorelines are aggregated accurately", {
  create_scorelines <- function(p00, p10, p01, p11) {
    tibble::tribble(
      ~hgoal, ~agoal, ~prob,
      0,      0,   p00,
      1,      0,   p10,
      0,      1,   p01,
      1,      1,   p11,
      2,      1,   1 - sum(p00, p10, p01, p11)
    )
  }

  expect_equal_scorelines <- function(p00, p10, p01, p11) {
    scorelines <- create_scorelines(p00, p10, p01, p11)

    p21 <- 1 - sum(p00, p10, p01, p11)
    hw <- p10 + p21
    d <- p00 + p11
    aw <- p01

    outcomes <- tibble::tribble(
      ~outcome, ~prob,
      "home_win",    hw,
      "draw",     d,
      "away_win",    aw
    )

    expect_equal(outcomes, scorelines_to_outcomes(scorelines))
  }

  expect_equal_scorelines(0.25, 0.25, 0.25, 0.15)
  expect_equal_scorelines(1.00, 0.00, 0.00, 0.00)
  expect_equal_scorelines(0.00, 1.00, 0.00, 0.00)
  expect_equal_scorelines(0.00, 0.00, 1.00, 0.00)

  incomplete_scorelines <- create_scorelines(0.25, 0.25, 0.25, 0.15)[1:4,]
  expect_warning(scorelines_to_outcomes(incomplete_scorelines), "Outcome probabilites expected to sum to 1")
})
