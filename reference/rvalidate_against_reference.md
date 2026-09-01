# Validate diagnostics against reference implementations

Compares the package's test results with established implementations
from other packages (currently `lmtest` and `car`). Differences
exceeding the supplied tolerance are flagged.

## Usage

``` r
rvalidate_against_reference(test_name, model, data, tolerance = 1e-06)
```

## Arguments

- test_name:

  Character scalar naming the diagnostic: "breusch_pagan",
  "studentized_bp", or "white".

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  representing the mean specification to be diagnosed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  object coercible to one) containing the variables referenced by
  `model`. It must include the observations used to fit `model` and will
  be checked for missing values.

- tolerance:

  Acceptable absolute difference between statistics and p-values when
  comparing to the reference implementation.

## Value

A list describing the comparison, including a status field with values
`"ok"`, `"mismatch"`, or `"skipped"` when the reference package is
unavailable.
