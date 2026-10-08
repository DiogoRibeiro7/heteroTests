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
`paper/simulation_study.R` and `paper/performance_study.R`.
