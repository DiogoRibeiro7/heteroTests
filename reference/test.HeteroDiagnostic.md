# Run diagnostic tests

Run the registered diagnostics on the model.

## Usage

``` r
test(object, ...)
```

## Arguments

- object:

  A `HeteroDiagnostic` object.

- ...:

  Further arguments passed to the individual tests.

## Value

List of test results.

## Deprecated

`test()` is deprecated since 0.14.0: its name masks `devtools::test()`.
Use `runDiagnostics(x$model, x$data, tests)`, which returns what
`test(x, tests)` returns. It still works and warns;
[`summary()`](https://rdrr.io/r/base/summary.html) of a
`HeteroDiagnostic` object is unchanged.
