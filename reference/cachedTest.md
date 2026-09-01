# Cache results of a heteroscedasticity test

Avoids recomputing expensive diagnostics by hashing their inputs and
reusing stored results.

## Usage

``` r
cachedTest(test_name, model, data, ..., use_cache = TRUE)
```

## Arguments

- test_name:

  Character scalar naming the diagnostic registered in
  `.diagnostic_registry` or `.test_factory`.

- model:

  Fitted model object supplied to the diagnostic.

- data:

  Data frame used to fit the model.

- ...:

  Additional arguments forwarded to the diagnostic.

- use_cache:

  Logical scalar indicating whether cached results may be used (defaults
  to `TRUE`).

## Value

The diagnostic result, either retrieved from cache or freshly computed.

## Details

The cache key is created by hashing the test name, model coefficients,
residuals, data, and additional arguments. When the key already exists
in the cache environment the stored result is returned immediately;
otherwise the diagnostic is executed and its output stored for future
calls. Set `use_cache = FALSE` to force recomputation when the
underlying model has changed.

## See also

[`clearTestCache()`](https://diogoribeiro7.github.io/heteroTests/reference/clearTestCache.md)
resets the cache between analysis sessions.
