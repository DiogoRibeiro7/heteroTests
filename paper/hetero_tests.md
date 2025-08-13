# Package Analysis: `heteroTests`

## 1. Metadata (DESCRIPTION)
- **Version**: 0.6.0  
- **License**: Apache-2.0  
- **Imports**:  
  - `stats` (base)  
- **Suggests**:  
  - `testthat`, `knitr`, `rmarkdown`, `car`, `sandwich`  
- **Overview**:  
  - Implements **25+** heteroscedasticity tests  
  - Unified interface under one namespace  

## 2. Core Structure
- **Directory Layout**  
  ```
  R/
    ├─ WhiteTest.R
    ├─ BPTest.R
    ├─ … (one file per test)
    ├─ runHeteroTests.R      # convenience wrapper
    └─ hetero_data.R         # example dataset
  tests/                     # internal algorithms and helpers
  man/                       # Roxygen2-generated documentation
  ```
- **S3 `htest` Class**  
  - All `perform*Test()` functions return an object of class `htest` with fields:  
    - `statistic`, `parameter`, `p.value`, `method`, `data.name`  
    - + test-specific metadata (e.g. `resid.type`, `lag`, `trim`)  
- **Optional Dependencies**  
  - If `car` is installed, uses its group-test implementations; otherwise uses internal fallbacks  

## 3. Implemented Tests (~25+)
- **Auxiliary-Regression Tests**:
  - White test
  - Breusch–Pagan test
  - Koenker (studentized BP) test
  - Harvey test
  - Park test
  - Glejser test
  - Spearman rank test
  - Cameron–Trivedi test
  - Ordered Lagrange Multiplier test
- **Group-wise / Nonparametric Tests**:
  - Goldfeld–Quandt test
  - Levene’s test
  - Brown–Forsythe test
  - Fligner–Killeen test
  - Bartlett’s test (and Bartlett’s modified statistic)
  - O’Brien’s test
  - Rice test
  - Curry–Walsh test
  - Hartley’s F<sub>max</sub> test
- **Non-constant Variance Diagnostics**:
  - Breusch–Pagan NCV test (Cook–Weisberg variant)
  - Spread–Level plot test (Moses test)
- **ARCH-Type / Time-Series Tests**:
  - ARCH LM (Engle) test
  - McLeod–Li test
- **Other Diagnostic Tests**:
  - Pesaran’s test
  - Conley–Zeldin test
  - Additional specialized tests (e.g., Glesjer variants)

## 4. Convenience Wrapper Convenience Wrapper
```r
runHeteroTests(model, tests = NULL, resid.type = "response", ...)
```
- **Default suite**: White, BP, Koenker, NCV, Spread–Level, Cook–Weisberg  
- **Arguments**:  
  - `tests`: character vector of test names  
  - `resid.type`, `trim`, `order.by`, `lag`, etc.  
- **Returns**: named list of `htest` objects  

## 5. Testing & CI
- **Unit testing**: `testthat` (coverage > 90%)  
- **CI**: GitHub Actions for Linux/macOS checks and pkgdown site build  

## 6. Next Steps for the Article
1. **Statistical Background**  
   - **Subsection structure**: Introduce notation (model: \(y = X\beta + \varepsilon\)), define \(\mathrm{Var}(\varepsilon_i) = \sigma_i^2\).  
   - **Test-by-test detail**: Include formulas for each test statistic:  
     - *White*: \(nR^2\) from auxiliary regression of squared residuals on regressors + cross-terms.  
     - *Breusch–Pagan*: LM statistic using score test formulation.  
     - *Goldfeld–Quandt*: F-ratio of subsample variances.  
   - **Notation table**: Clarify symbols (e.g., \(e_i\), \(R^2\), regressors \(k\), sample size \(n\)).  
2. **Full “Implemented Tests” Table**  
   - **Columns**: Test name, classification, key assumption(s), distribution under \(H_0\), typical application.  
   - **Layout**: Use LaTeX `\begin{tabular}` template for ~25 entries, grouped by test family.  
3. **Package Design Details**  
   - **S3 Class Example**: Show code from `performWhiteTest()` illustrating `htest` object construction and `print.htest` method.  
   - **Directory Layout Diagram**: ASCII or TikZ diagram of `R/`, `man/`, `tests/`, `vignettes/`, `data/`.  
   - **Code snippet**: Excerpt of input validation and output formatting from one test file.  
4. **Usage & Vignettes**  
   - **`runHeteroTests()`**: Example customizing `tests = c("White", "Levene")`, iterating over results list.  
   - **Vignette extract**: YAML header and introductory analysis text for `vignettes/heteroTests-workflow.Rmd`.  
5. **Simulation & Benchmarks**  
   - **Scenarios**:  
     - *Continuous heteroscedasticity*: \(\sigma^2(x)=1+\alpha x\) for \(\alpha\in\{0,1,2\}\).  
     - *Step variance*: Low/high regimes split by median of \(x\).  
     - *ARCH(1)*: \(\sigma_t^2=\omega+\alpha\varepsilon_{t-1}^2\).  
   - **Metrics**: Power curves, type I error, execution time scaling.  
   - **Outputs**: Plan power-curve figures, timing tables, and interpretive comments.  
6. **Comparison**  
   - **Feature Matrix**: R code to build a comparison table of package vs. tests implemented.  
   - **Syntax Comparison**: Side-by-side code for `lmtest::bptest()`, `car::leveneTest()`, and `performBPTest()`.  
   - **Discussion**: Emphasize unified API, rich metadata, minimal dependencies, and fallback behavior.

