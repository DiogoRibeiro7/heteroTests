# Clear cached diagnostic run results

Removes memoised outputs stored by
[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
when called with `use_cache = TRUE`. Useful for deterministic test runs
or after mutating the underlying data/model so cached summaries should
be invalidated.

## Usage

``` r
clearAnalysisCache()
```

## See also

[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md),
[`clearTestCache`](https://diogoribeiro7.github.io/heteroTests/reference/clearTestCache.md)
