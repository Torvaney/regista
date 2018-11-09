context("Dixon-Coles methods")


test_that("Models can be printed", {
  fit_simple <- dixoncoles(hgoal, agoal, home, away, premier_league_2010)

  output <- capture_output(print(fit_simple))
  expected_output <- glue::glue(
    "

    Dixon-Coles model with specification:

    Home goals: hgoal ~ off(home) + def(away) + hfa + 0
    Away goals: agoal ~ off(away) + def(home) + 0
    Weights   : 1

    "
  )

  expect_equal(output, as.character(expected_output))
})
