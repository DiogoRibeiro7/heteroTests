# Interactive diagnostic dashboard

Launch a production-ready Shiny dashboard for running heteroscedasticity
diagnostics, exploring interactive plots, and experimenting with
simulation studies.

## Usage

``` r
launchDiagnosticDashboard(model, data)
```

## Arguments

- model:

  Fitted `lm` model.

- data:

  Data frame used to fit the model.

## Value

A `shiny.appobj` that can be run with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

Requires optional packages shiny, DT, and plotly. Excel uploads
additionally depend on readxl and exporting interactive graphics relies
on htmlwidgets. The dashboard guides users through data preparation,
model fitting, diagnostic testing, interactive Plotly visualisations,
and a real-time simulation lab for exploring heteroscedastic
data-generating processes. Download buttons are provided for both
results tables and interactive plots.

## Examples

``` r
if (FALSE) { # \dontrun{
data(mtcars)
mod <- lm(mpg ~ wt + hp, data = mtcars)
launchDiagnosticDashboard(mod, mtcars)
} # }
```
