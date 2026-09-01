# Download example teaching dataset

Provides convenient access to example datasets hosted online. Currently
supports the 'tips' dataset used in various tutorials.

## Usage

``` r
downloadTeachingData(
  name = "tips",
  destfile = tempfile(fileext = ".csv"),
  quiet = FALSE
)
```

## Arguments

- name:

  Name of the dataset to download. Only "tips" is currently supported.

- destfile:

  Optional path to save the downloaded dataset. Defaults to a temporary
  file.

- quiet:

  Logical; if FALSE, informative messages are printed during the
  download process.

## Value

Path to the downloaded dataset on success, or `NULL` invisibly if the
download fails or no internet connection is available.

## Examples

``` r
# \donttest{
# Online example (requires internet)
if (curl::has_internet()) {
  data_path <- downloadTeachingData(quiet = TRUE)
  if (!is.null(data_path)) {
    # Use downloaded data
  }
}
#> NULL
# }

# Offline example using built-in data
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)
result <- performWhiteTest(model, mtcars)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5431 df = 5 p = 0.2569
print(result)
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  model
#> X-squared = 6.5431, df = 5, p-value = 0.2569
#> alternative hypothesis: heteroscedasticity present
#> 
```
