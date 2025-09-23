# Benchmark Outputs

This directory stores machine-generated benchmark artefacts produced by
`scripts/run_benchmarks.R`. All CSV files are ignored from version control to
keep the repository lightweight. To reproduce the results:

```sh
Rscript scripts/run_benchmarks.R
```

Use the `--quick` flag for a shorter run or `--no-memory` to skip memory
profiling when the `bench` package is unavailable.
