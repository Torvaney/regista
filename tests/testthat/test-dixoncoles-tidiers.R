context("Dixon-Coles tidiers")


test_that("Models can be tidied", {
  fit_simple <- dixoncoles(hgoal, agoal, home, away, premier_league_2010)

  tidied <- broom::tidy(fit_simple)

  # We don't care about the values
  pl_teams <- unique(as.character(premier_league_2010$home, premier_league_2010$away))
  td_teams <- unique(tidied$team)
  expect_equivalent(c(pl_teams, NA_character_), td_teams)

  expect_type(tidied$value, "double")

  pars <- unique(tidied$parameter)
  expect_equivalent(c("off", "def", "hfa", "rho"), pars)
})
