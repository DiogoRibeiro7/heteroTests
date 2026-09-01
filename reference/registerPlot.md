# Register a diagnostic plot

Register a diagnostic plot

## Usage

``` r
registerPlot(name, fun)
```

## Arguments

- name:

  Name of the plot

- fun:

  Function taking a model and returning a ggplot object

## Value

Invisibly returns `NULL`.

## Examples

``` r
registerPlot("custom_plot", function(model) ggplot2::ggplot())
```
