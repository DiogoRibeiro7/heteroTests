# Withdrawn Curry-Walsh spatial pseudo-test

Withdrawn in 0.7.2. The statistic was never standardised, so the test
could not reject; the function signals a migration error rather than
returning invalid inference.

## Usage

``` r
performCurryWalshTest(model, coords, ...)
```

## Details

The former implementation computed Moran's I on the squared residuals
with inverse-distance weights and passed the raw statistic to
`2 * (1 - pnorm(abs(I)))`. Under the null, Moran's I has expectation
\\-1/(n-1)\\ and a variance determined by the weights matrix, so
inference requires the standardised \\z = (I - E\[I\]) /
\sqrt{\mathrm{Var}(I)}\\ or a permutation reference. Because I is
bounded near \\\pm 1\\ it essentially never reached 1.96: the test
rejected in 0.0% of samples generated with strong heteroscedasticity,
and returned \\I = 0.157\\ with \\p = 0.875\\ on clustered variance.

[`performSpatialHeteroTest`](https://diogoribeiro7.github.io/heteroTests/reference/performSpatialHeteroTest.md)
already provides this diagnostic correctly, applying
[`spdep::moran.mc()`](https://r-spatial.github.io/spdep/reference/moran.mc.html)
to the squared residuals so that the reference distribution comes from
permutation rather than an unstandardised normal approximation.

## Arguments

- model:

  an object of class `lm`. Retained only for backward-compatible
  argument matching.

- coords:

  two-column coordinate matrix or data frame. Retained only for
  backward-compatible argument matching.

- ...:

  further arguments, ignored.

## Value

This function does not return a test result. It signals an error with
migration guidance.

## References

Moran, P. A. P. (1950). Notes on continuous stochastic phenomena.
*Biometrika*, 37(1/2), 17–23.
[doi:10.2307/2332142](https://doi.org/10.2307/2332142)

## See also

[`performSpatialHeteroTest`](https://diogoribeiro7.github.io/heteroTests/reference/performSpatialHeteroTest.md)
