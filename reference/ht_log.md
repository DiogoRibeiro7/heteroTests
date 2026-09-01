# Log a formatted message

This helper wraps
[`base::message()`](https://rdrr.io/r/base/message.html) but prepends a
log level for clearer diagnostics when running algorithms. Messages are
filtered according to
[`ht_set_log_level()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_logging.md)
and can optionally be captured with
[`ht_enable_log_capture()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_logging.md).
Intended for internal use.

## Usage

``` r
ht_log(level = c("INFO", "WARN", "ERROR"), msg)
```

## Arguments

- level:

  One of "INFO", "WARN" or "ERROR".

- msg:

  Character string with the message to log.

## Value

`NULL`, invoked for its side effect.
