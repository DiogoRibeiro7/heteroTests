# Provenance: derived verbatim from MASS::Boston (the classic Boston housing data).
# Shipped so the package's examples, tests and tutorials run without MASS attached.
boston_housing <- MASS::Boston
usethis::use_data(boston_housing, overwrite = TRUE)
