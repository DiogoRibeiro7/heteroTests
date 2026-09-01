# Control logging for heteroTests diagnostics

These helpers manage the package-level logging subsystem used to debug
complex diagnostic workflows. Users can adjust verbosity, enable log
capture, inspect recent log entries and clear the stored history when
finished.

## Usage

``` r
ht_set_log_level(level = c("INFO", "WARN", "ERROR", "SILENT"))

ht_enable_log_capture(enabled = TRUE, max_entries = 1000L, sink = NULL)

ht_log_history()

ht_clear_log_history()
```

## Arguments

- level:

  Character string specifying the minimum level to emit. Accepted values
  are "INFO", "WARN", "ERROR" and "SILENT".

- enabled:

  Logical flag, `TRUE` to enable capture and `FALSE` to disable.

- max_entries:

  Maximum number of entries to retain in memory. Older entries are
  discarded first. Use `Inf` to keep all entries.

- sink:

  Optional file path or connection to mirror log output.

## Value

`ht_set_log_level()` returns the previous log level invisibly.
`ht_enable_log_capture()` and `ht_clear_log_history()` return `NULL`.
`ht_log_history()` returns a data frame with columns `timestamp`,
`level` and `message`.
