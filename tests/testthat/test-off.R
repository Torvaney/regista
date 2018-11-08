context("Model specification")

test_that("Generating offense and defence dummies works", {
  teams <- factor(c("Arsenal", "Liverpool"))

  off_teams <- factor(c("off___Arsenal", "off___Liverpool"))
  def_teams <- factor(c("def___Arsenal", "def___Liverpool"))

  expect_equal(off(teams), off_teams)
  expect_equal(def(teams), def_teams)
})
