context("Utils")

test_that("Generating offense and defence dummies works", {
  teams <- factor(c("Arsenal", "Liverpool"))

  off_teams <- factor(c("off___Arsenal", "off___Liverpool"))
  def_teams <- factor(c("def___Arsenal", "def___Liverpool"))

  expect_equal(off(teams), off_teams)
  expect_equal(def(teams), def_teams)
})


test_that("Scorelines are aggregated accurately", {
  scorelines <- tibble::tribble(
    ~hgoal, ~agoal, ~prob,
         0,      0,  0.25,
         1,      0,  0.25,
         0,      1,  0.25,
         1,      1,  0.15,
         2,      1,  0.10,
  )

  outcomes <- tibble::tribble(
      ~outcome, ~prob,
    "home_win",  0.35,
        "draw",  0.40,
    "away_win",  0.25
  )

  expect_equal(outcomes, scorelines_to_outcomes(scorelines))
})
