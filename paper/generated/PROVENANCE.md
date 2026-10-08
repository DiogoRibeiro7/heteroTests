# R Journal empirical-result provenance

The files in this directory are compact manuscript inputs extracted from successful GitHub Actions artifacts.

## Simulation

- Workflow run: 37694496146
- Artifact: 11515714496 (`r-journal-simulation-results`)
- Source commit: fc491135323370a2be9751e3e32a85fe10e7ff1c
- Package: heteroTests 0.11.2
- R: 4.6.1
- Platform: x86_64-pc-linux-gnu, Ubuntu 24.04.5 LTS
- Seed: 20261007
- Replications per cell: 2000
- Alpha: 0.05

Committed extracts:
- `simulation_size_table.csv`
- `simulation_power_full_strength.csv`

## Performance

- Workflow run: 37698054532
- Artifact: 11516352264 (`r-journal-performance-results`)
- Source commit: 8e6d6c3ac169bd3925490c19311d16bfa2e0cdd0
- Package: heteroTests 0.11.2
- R: 4.6.1
- Platform: x86_64-pc-linux-gnu, Ubuntu 24.04.5 LTS
- Seed: 20261007
- Streaming chunk size: 5000
- Memory profiler: bench

Committed extracts:
- `performance_reference_speed.csv`
- `performance_accuracy_table.csv`
- `performance_streaming_summary.csv`

The long-running source experiments remain reproducible through
`paper/scripts/simulation_study.R` and `paper/scripts/performance_study.R`.

## Package version described by the article

The article describes heteroTests 0.12.0. The simulation and performance
extracts above were generated with 0.11.2 and were not regenerated.

Version 0.12.0 changes the four simulated procedures (White, Breusch-Pagan,
Koenker, Goldfeld-Quandt) only for weighted fits, and the study fits
unweighted models. As a check, `paper/scripts/simulation_study.R` was run with
150 replications per cell under 0.11.2 and under 0.12.0 with the script's own
seed. The two `simulation_results.csv` files were identical in all 168 rows.

- R: 4.3.3
- Platform: x86_64-pc-linux-gnu, Ubuntu 24.04.5 LTS

## Weighted fits and the variance-function test

The rejection rates the article quotes for weighted fits and for
`performVarianceFormTest()` are not stored in this directory. The article
reads them at render time from the installed package, through
`system.file("validation", "weighted-fits-size.csv", package = "heteroTests")`,
so they belong to the package version being rendered.

- Script: `inst/validation/weighted-fits-size.R`
- Package: heteroTests 0.12.0
- R: 4.3.3
- Platform: x86_64-pc-linux-gnu, Ubuntu 24.04.5 LTS
- Seeds: 20261008 (known weights), 20261009 (weights estimated by `fitWLS()`),
  20261010 (wrong variance function)
- Replications per cell: 400 (known weights), 2000 (the other two blocks)
- Alpha: 0.05
