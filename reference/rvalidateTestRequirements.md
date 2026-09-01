# Validate test-specific requirements

Aggregates validation logic tailored to the supplied `test_name` and
returns a summary of any problems detected.

## Usage

``` r
rvalidateTestRequirements(test_name, model, data, ...)
```

## Arguments

- test_name:

  Name of the heteroscedasticity test whose requirements are being
  validated.

- model:

  Fitted model object used by the diagnostic. Required for checks that
  depend on model residuals (e.g. bootstrap procedures).

- data:

  Data frame containing the variables required by the test.

- ...:

  Additional arguments that refine the checks for particular tests.

## Value

Validation result combining all relevant requirements.
