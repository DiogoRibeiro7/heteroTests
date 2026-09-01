# Enhanced model diagnostics

Extends
[`checkModel()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModel.md)
with additional soft checks that flag potential numerical issues before
computationally expensive diagnostics are executed. The helper mirrors
the warnings used throughout the package and retains the original return
semantics for backward compatibility.

## Usage

``` r
checkModelEnhanced(model, data = NULL)
```

## Arguments

- model:

  Fitted model supplied to subsequent diagnostics.

- data:

  Optional `data.frame` used when fitting `model`. When provided it
  enables multicollinearity checks via
  [`performVIFDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performVIFDiagnostic.md).

## Value

Invisibly returns `model` after issuing any relevant warnings.

## Details

The function warns when residual degrees of freedom fall below six, when
a near-perfect fit is detected (R\\^2 \> 0.999\\), or when variance
inflation factors (VIFs) exceed 10. The latter threshold follows the
regression diagnostics literature (Belsley et al., 1980).

## References

Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity*.
Wiley.

## See also

[`checkModel()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModel.md),
[`performVIFDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performVIFDiagnostic.md),
[`validateTestInputs()`](https://diogoribeiro7.github.io/heteroTests/reference/validateTestInputs.md)

## Examples

``` r
data(mtcars)
fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
checkModelEnhanced(fit)
```
