# regista 0.3.2.9000 (In progress)

* S3 method for `broom::tidy` and `broom::augment`
* Standardise `predict.dixoncoles` to always return a list of `tibble::tibbles`
no matter the `type`.

# regista 0.3.1

* Allow dixoncoles predictions on data without home/away goal columns

# regista 0.3.0

* Change `dixoncoles` API to use unquoted variable names.
* Allow outcome probabilities to be predicted directly
* Return tibbles over data.frames where appropriate

# regista 0.2.0 and below

* `dixoncoles` and `dixoncoles_ext` function for estimating team strengths.
* `print` and `predict` methods for dixoncoles models.
* `factor_teams` helper function.
* `scorelines_to_outcomes` helper function to easily calculate home/draw/win
probabilities.
* time-discounting functions as in the original Dixon-Coles paper.
