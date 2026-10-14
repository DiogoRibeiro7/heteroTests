# Validate model argument

Checks that `model` is of class `lm` or `glm`. With `enhanced = TRUE` it
also runs the soft checks of the deprecated
[`checkModelEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModelEnhanced.md):
it warns when the residual degrees of freedom fall below six, when a
near-perfect fit is detected (R\\^2 \> 0.999\\), or, when `data` is
given, when a variance inflation factor exceeds 10 (Belsley et al.,
1980).

## Usage

``` r
checkModel(model, data = NULL, enhanced = FALSE)
```

## Arguments

- model:

  An object to check.

- data:

  Optional `data.frame` used when fitting `model`. With
  `enhanced = TRUE` it enables the multicollinearity check through
  [`performVIFDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performVIFDiagnostic.md).

- enhanced:

  Logical. `FALSE`, the default, checks the class only.

## Value

Invisible `model` if valid, otherwise an error is thrown.

## References

Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity*.
Wiley.
