# regista 0.4.1.9000 (In progress)

* Suppress warnings in `dixoncoles` caused by insufficent bounds on parameter 
estimates (see #1 and #23)
* Normalise attack parameters during fitting (see #1)

# regista 0.4.0

* S3 methods for `broom::tidy` and `broom::augment`
* Standardise `predict.dixoncoles` to always return a list of `tibble::tibble`s
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
