# S3 containers for heteroscedasticity diagnostics

Collections of heteroscedasticity test results returned by
`runHeteroTests`. A `hetero_test_suite` is a simple named list of
`htest` objects with additional metadata. When diagnostics are evaluated
on grouped data, the returned object inherits from
`hetero_grouped_suite` and contains one `hetero_test_suite` per group
along with the grouping keys.

## Details

These classes are primarily useful for method dispatch (e.g. the broom
tidiers and `autoplot()` methods). Users typically interact with the
objects via
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`generics::glance()`](https://generics.r-lib.org/reference/glance.html),
[`generics::augment()`](https://generics.r-lib.org/reference/augment.html)
and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
