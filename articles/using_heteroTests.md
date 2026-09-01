# Using heteroTests

The **heteroTests** package provides several classical tests for
detecting heteroscedasticity in linear models.

``` r

library(heteroTests)

# Fit a simple linear model on real data
data(boston_housing, package = "heteroTests")
model <- lm(medv ~ lstat + rm + crim, data = boston_housing)
# A second dataset demonstrates collinearity and mild nonlinearity
data(diagnostic_data)
diag_model <- lm(y ~ x1 + x2, data = diagnostic_data)

# Run White's test and get an htest object
performWhiteTest(model, boston_housing)
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running White.
# [INFO] Running White test
# [INFO] White test completed: statistic = 135.1481 df = 9 p = 0
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
performWhiteTest(diag_model, diagnostic_data)
# [INFO] Running White test
# [INFO] White test completed: statistic = 1.5791 df = 5 p = 0.9038
# 
#   White's test for heteroscedasticity
# 
# data:  diag_model
# X-squared = 1.5791, df = 5, p-value = 0.9038
# alternative hypothesis: heteroscedasticity present
```

## Other available tests

Many additional diagnostics follow the same interface and also return an
`htest` object.

``` r

# Breusch-Pagan and its robust Koenker version. The data argument must be
# the data the model was fitted on.
performBPTest(model, boston_housing)
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Breusch-Pagan.
# [INFO] Running Breusch-Pagan test
# 
#   Breusch-Pagan test for heteroscedasticity
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 24.344, df = 3, p-value = 2.117e-05
performKoenkerTest(model, boston_housing)
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Koenker.
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Koenker studentized Breusch-Pagan test.
# [INFO] Running Koenker test
# 
#   Koenker studentized Breusch-Pagan test
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 7.4741, df = 3, p-value = 0.05823

# Group-based tests need a grouping factor in that same data
boston_grouped <- boston_housing
boston_grouped$chas <- factor(boston_grouped$chas)
performLeveneTest(model, boston_grouped, "chas")
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Levene.
# 
#   Levene's test for equality of variances
# 
# data:  medv ~ lstat + rm + crim
# F = 8.4233, df1 = 1, df2 = 504, p-value = 0.003867

# ARCH effects in time series
performArchLMTest(model, lags = 2)
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running ARCH LM.
# [INFO] Running ARCH LM test
# 
#   Engle's ARCH LM test
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 174.08, df = 2, p-value < 2.2e-16
```

Several diagnostics can also be run together.

``` r

# Alternatively run multiple tests at once
runHeteroTests(model, boston_housing)
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running White.
# [INFO] Running White test
# [INFO] White test completed: statistic = 135.1481 df = 9 p = 0
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Breusch-Pagan.
# [INFO] Running Breusch-Pagan test
# $white
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
# 
# 
# $breusch_pagan
# 
#   Breusch-Pagan test for heteroscedasticity
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 24.344, df = 3, p-value = 2.117e-05
# 
# 
# attr(,"class")
# [1] "hetero_test_suite" "list"             
# attr(,"tests")
# [1] "white"         "breusch_pagan"
# attr(,"model")
# 
# Call:
# lm(formula = medv ~ lstat + rm + crim, data = boston_housing)
# 
# Coefficients:
# (Intercept)        lstat           rm         crim  
#     -2.5623      -0.5785       5.2170      -0.1029  
# 
# attr(,"data_source")
# [1] "data.frame"
# Choose a subset of diagnostics
runHeteroTests(model, boston_housing, tests = c("white", "koenker", "ncv"))
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Koenker.
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Koenker studentized Breusch-Pagan test.
# [INFO] Running Koenker test
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running NCV.
# [INFO] Running NCV score test
# $white
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
# 
# 
# $koenker
# 
#   Koenker studentized Breusch-Pagan test
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 7.4741, df = 3, p-value = 0.05823
# 
# 
# $ncv
# 
#   Cook-Weisberg score test for non-constant variance
# 
# data:  medv ~ lstat + rm + crim; variance model: fitted values
# X-squared = 0.23549, df = 1, p-value = 0.6275
# alternative hypothesis: error variance depends on the variance model
# 
# 
# attr(,"class")
# [1] "hetero_test_suite" "list"             
# attr(,"tests")
# [1] "white"   "koenker" "ncv"    
# attr(,"model")
# 
# Call:
# lm(formula = medv ~ lstat + rm + crim, data = boston_housing)
# 
# Coefficients:
# (Intercept)        lstat           rm         crim  
#     -2.5623      -0.5785       5.2170      -0.1029  
# 
# attr(,"data_source")
# [1] "data.frame"
runDiagnostics(model, boston_housing)
# $white
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
# 
# 
# $breusch_pagan
# 
#   Breusch-Pagan test for heteroscedasticity
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 24.344, df = 3, p-value = 2.117e-05
# 
# 
# $vif
#    lstat       rm     crim 
# 1.941883 1.616468 1.271372 
# 
# $reset
# 
#   RESET test for nonlinearity
# 
# data:  medv ~ lstat + rm + crim
# F = 133.3, df1 = 2, df2 = 500, p-value < 2.2e-16
# 
# 
# $influence
# $influence$cooks_distance
#            1            2            3            4            5            6 
# 8.844300e-04 3.516092e-04 2.175317e-04 6.563043e-05 9.082876e-04 2.143734e-05 
#            7            8            9           10           11           12 
# 3.967455e-05 3.666575e-03 8.786128e-03 7.949884e-08 1.095939e-03 1.122697e-04 
#           13           14           15           16           17           18 
# 2.009410e-04 4.416653e-04 6.289417e-04 4.438218e-04 1.097916e-04 1.603822e-04 
#           19           20           21           22           23           24 
# 7.842835e-05 2.519196e-04 2.018634e-05 1.735826e-05 4.576513e-04 1.267555e-04 
#           25           26           27           28           29           30 
# 2.948807e-04 3.658158e-04 1.802132e-04 5.251257e-04 6.487540e-04 4.488255e-04 
#           31           32           33           34           35           36 
# 1.231037e-04 9.551140e-04 1.177512e-04 4.276813e-04 7.573463e-04 5.289224e-04 
#           37           38           39           40           41           42 
# 5.556258e-05 1.609438e-04 1.266137e-04 8.843139e-05 2.152792e-04 4.087460e-04 
#           43           44           45           46           47           48 
# 4.218433e-05 2.250707e-05 1.610115e-04 1.701181e-04 1.023622e-05 8.421573e-05 
#           49           50           51           52           53           54 
# 8.160093e-03 1.770417e-04 2.732526e-05 3.145688e-04 4.135276e-04 7.653962e-06 
#           55           56           57           58           59           60 
# 1.316865e-05 4.258406e-04 2.725741e-04 3.294636e-05 1.976763e-04 4.410571e-04 
#           61           62           63           64           65           66 
# 3.815822e-05 4.366141e-04 7.481973e-04 1.235752e-04 4.001539e-04 8.220715e-04 
#           67           68           69           70           71           72 
# 2.190147e-04 9.651176e-05 1.146228e-04 1.978383e-04 2.437556e-04 3.964907e-05 
#           73           74           75           76           77           78 
# 5.346878e-04 1.576925e-04 1.591593e-04 3.203473e-04 2.065452e-04 1.760576e-04 
#           79           80           81           82           83           84 
# 5.121492e-05 2.641387e-04 7.368438e-05 3.954995e-04 9.023810e-05 1.954686e-04 
#           85           86           87           88           89           90 
# 3.493076e-05 7.747596e-05 2.955877e-05 1.656523e-04 1.958319e-03 2.229951e-04 
#           91           92           93           94           95           96 
# 2.304041e-04 4.104445e-04 2.807631e-04 6.460420e-05 2.261354e-04 1.899757e-06 
#           97           98           99          100          101          102 
# 5.505975e-05 3.389797e-04 5.672558e-03 2.633987e-05 4.539368e-06 9.353365e-05 
#          103          104          105          106          107          108 
# 7.118649e-04 1.174577e-04 1.134323e-04 3.675734e-05 2.432479e-04 1.571549e-05 
#          109          110          111          112          113          114 
# 3.964802e-04 6.243574e-05 5.650907e-06 3.497115e-04 2.726298e-07 1.235715e-05 
#          115          116          117          118          119          120 
# 6.272276e-04 2.428907e-05 4.428629e-05 3.776565e-04 4.324922e-05 7.308787e-07 
#          121          122          123          124          125          126 
# 1.401613e-04 9.474775e-07 2.052529e-04 1.804560e-03 2.706062e-05 4.361356e-05 
#          127          128          129          130          131          132 
# 2.949644e-03 3.476889e-05 5.212076e-04 1.385045e-04 4.538871e-04 2.357270e-04 
#          133          134          135          136          137          138 
# 2.615475e-05 1.343044e-05 1.040309e-04 2.366481e-04 4.735070e-05 8.663625e-04 
#          139          140          141          142          143          144 
# 3.329941e-04 4.152947e-05 2.968991e-04 2.788893e-02 1.342012e-03 2.673450e-03 
#          145          146          147          148          149          150 
# 5.188189e-03 7.930781e-05 6.122772e-05 1.144995e-02 1.252352e-02 1.095072e-04 
#          151          152          153          154          155          156 
# 3.962842e-06 1.819812e-04 1.479533e-04 7.023740e-05 2.729071e-04 4.485093e-04 
#          157          158          159          160          161          162 
# 2.877844e-04 4.370926e-03 4.224920e-05 3.534709e-04 3.351876e-06 1.605773e-02 
#          163          164          165          166          167          168 
# 1.710865e-02 2.114626e-02 8.496575e-05 7.709334e-05 2.041259e-02 2.305377e-04 
#          169          170          171          172          173          174 
# 5.155087e-08 5.176452e-05 1.228153e-04 8.968608e-05 1.068009e-03 9.304234e-05 
#          175          176          177          178          179          180 
# 1.309306e-06 2.919045e-05 1.292525e-06 1.668328e-04 1.409285e-05 1.509902e-03 
#          181          182          183          184          185          186 
# 3.959119e-03 3.914204e-03 1.587290e-03 5.681657e-04 2.456606e-03 1.237646e-03 
#          187          188          189          190          191          192 
# 2.028958e-02 2.716310e-04 2.683731e-05 4.363887e-04 1.469248e-03 1.462864e-05 
#          193          194          195          196          197          198 
# 5.415198e-04 4.306743e-05 2.668743e-06 1.796883e-02 2.368433e-06 2.441474e-05 
#          199          200          201          202          203          204 
# 4.630292e-04 5.542604e-04 3.015324e-05 5.001876e-05 3.745162e-03 1.529787e-02 
#          205          206          207          208          209          210 
# 1.857500e-02 1.717339e-05 2.083709e-06 1.105040e-03 3.500913e-04 5.141691e-03 
#          211          212          213          214          215          216 
# 3.385643e-04 4.913770e-03 4.891367e-04 1.719512e-04 3.709956e-02 1.635101e-05 
#          217          218          219          220          221          222 
# 2.362200e-04 1.104064e-04 4.278908e-04 4.995494e-05 6.093885e-05 1.501390e-03 
#          223          224          225          226          227          228 
# 8.295383e-09 1.673587e-04 7.139594e-03 2.450663e-02 5.705509e-07 1.136527e-05 
#          229          230          231          232          233          234 
# 1.096374e-02 2.095392e-04 1.517395e-04 1.040274e-04 8.488264e-04 1.616412e-02 
#          235          236          237          238          239          240 
# 3.455832e-05 3.066234e-05 4.001722e-05 1.327959e-04 4.650385e-04 4.805405e-04 
#          241          242          243          244          245          246 
# 8.347880e-04 8.193197e-05 6.983888e-05 6.854504e-04 1.395799e-04 2.820386e-04 
#          247          248          249          250          251          252 
# 3.152304e-06 2.709590e-04 1.950347e-05 1.757401e-04 4.014286e-04 9.019086e-04 
#          253          254          255          256          257          258 
# 1.948791e-04 2.988109e-03 5.776733e-04 1.382811e-04 5.906357e-03 2.649946e-02 
#          259          260          261          262          263          264 
# 1.323318e-03 3.124117e-05 9.660620e-04 8.170861e-03 2.378422e-02 2.607340e-04 
#          265          266          267          268          269          270 
# 1.789868e-03 3.574998e-04 1.840168e-03 3.506711e-02 5.401318e-03 2.066509e-06 
#          271          272          273          274          275          276 
# 1.233517e-05 3.486380e-05 1.744674e-04 1.902328e-04 1.374317e-04 1.395186e-05 
#          277          278          279          280          281          282 
# 9.134669e-05 2.452443e-04 1.101278e-04 8.994829e-04 8.496435e-03 7.270597e-04 
#          283          284          285          286          287          288 
# 8.737362e-03 1.854922e-02 2.108702e-04 4.459464e-04 1.137340e-04 2.204446e-04 
#          289          290          291          292          293          294 
# 3.919113e-04 4.044504e-05 3.574685e-04 1.058976e-03 7.554282e-05 8.670434e-06 
#          295          296          297          298          299          300 
# 3.178015e-05 3.887028e-08 1.280258e-06 1.057750e-04 1.196028e-03 2.416158e-04 
#          301          302          303          304          305          306 
# 8.024747e-04 4.013881e-04 2.356086e-07 1.659574e-04 1.151504e-03 5.853087e-05 
#          307          308          309          310          311          312 
# 6.126025e-05 1.064481e-05 1.714073e-03 1.947086e-04 1.013509e-05 6.841838e-04 
#          313          314          315          316          317          318 
# 1.683386e-04 4.462414e-04 1.310739e-04 7.622177e-04 8.193619e-07 6.587399e-05 
#          319          320          321          322          323          324 
# 4.925570e-05 1.719524e-05 2.491439e-04 4.029244e-04 6.672045e-04 1.421605e-04 
#          325          326          327          328          329          330 
# 1.858561e-04 4.795729e-04 5.370169e-04 4.362190e-06 3.319916e-04 3.898823e-04 
#          331          332          333          334          335          336 
# 5.455701e-04 3.182469e-04 9.909742e-04 9.763435e-04 1.115058e-03 3.942553e-04 
#          337          338          339          340          341          342 
# 3.185216e-04 4.257727e-04 4.271210e-04 5.173927e-04 7.080802e-04 2.119588e-05 
#          343          344          345          346          347          348 
# 2.246889e-03 4.968805e-04 1.248104e-05 7.511594e-04 3.670878e-04 6.608075e-04 
#          349          350          351          352          353          354 
# 5.187241e-04 4.560976e-04 8.050246e-04 6.874789e-04 1.269240e-03 1.081551e-06 
#          355          356          357          358          359          360 
# 1.146015e-03 1.385284e-03 2.542854e-05 1.876568e-05 4.841101e-06 1.990030e-05 
#          361          362          363          364          365          366 
# 2.130953e-05 4.190971e-05 2.302168e-04 9.999044e-05 8.866643e-02 1.602101e-01 
#          367          368          369          370          371          372 
# 5.055707e-03 8.458902e-02 2.287920e-01 2.431819e-02 2.263697e-02 2.823846e-02 
#          373          374          375          376          377          378 
# 4.617108e-02 2.720090e-02 9.006181e-02 1.638607e-02 1.189021e-03 3.903946e-03 
#          379          380          381          382          383          384 
# 3.066173e-04 2.180138e-03 6.126804e-02 4.009264e-03 9.950492e-06 7.722725e-05 
#          385          386          387          388          389          390 
# 1.266343e-02 4.137364e-04 1.029602e-02 3.642113e-03 5.488101e-03 6.309435e-05 
#          391          392          393          394          395          396 
# 6.608891e-05 9.554045e-04 3.513101e-04 8.822601e-04 8.639494e-04 1.889009e-03 
#          397          398          399          400          401          402 
# 2.030117e-03 1.493381e-03 1.817111e-04 1.556224e-03 3.764478e-03 6.150218e-03 
#          403          404          405          406          407          408 
# 1.964810e-03 1.317068e-03 1.965182e-03 4.300851e-03 1.691781e-02 6.228249e-03 
#          409          410          411          412          413          414 
# 3.573677e-03 5.111843e-03 1.651627e-03 1.401685e-04 6.133840e-02 8.628304e-03 
#          415          416          417          418          419          420 
# 7.375963e-02 4.880727e-03 1.448958e-02 1.676176e-03 8.116982e-05 1.303394e-02 
#          421          422          423          424          425          426 
# 6.063831e-04 4.754423e-04 5.778351e-04 1.906904e-04 5.724495e-04 1.310566e-03 
#          427          428          429          430          431          432 
# 2.076019e-03 1.540037e-02 1.647050e-03 3.770956e-03 8.068005e-04 3.666093e-03 
#          433          434          435          436          437          438 
# 1.036434e-03 1.274290e-03 2.578912e-03 1.726739e-03 5.069375e-03 1.725791e-03 
#          439          440          441          442          443          444 
# 2.653510e-04 2.216424e-06 4.802200e-04 1.036050e-04 3.917928e-05 7.511803e-04 
#          445          446          447          448          449          450 
# 2.754776e-04 1.892377e-03 7.099893e-04 1.327721e-03 5.322785e-04 1.656114e-03 
#          451          452          453          454          455          456 
# 3.931962e-03 1.909585e-03 3.853354e-04 7.275801e-03 2.209546e-03 1.876754e-03 
#          457          458          459          460          461          462 
# 6.250209e-04 4.438266e-04 6.700703e-04 3.407380e-08 1.596854e-03 3.547041e-04 
#          463          464          465          466          467          468 
# 8.533945e-05 4.778956e-04 1.294172e-08 2.323751e-05 1.660639e-05 4.845874e-04 
#          469          470          471          472          473          474 
# 3.986063e-04 3.835791e-04 5.632020e-07 1.016027e-04 1.564403e-05 3.904395e-04 
#          475          476          477          478          479          480 
# 1.671795e-05 2.339445e-04 5.056293e-04 6.182399e-04 4.185972e-04 1.329940e-05 
#          481          482          483          484          485          486 
# 8.040202e-07 4.817225e-04 9.314928e-04 1.611772e-05 6.004931e-06 1.343352e-04 
#          487          488          489          490          491          492 
# 1.782949e-05 8.090930e-06 2.577414e-06 1.986397e-03 2.662820e-04 8.035713e-04 
#          493          494          495          496          497          498 
# 1.593268e-05 9.362844e-05 4.096794e-04 1.554364e-03 2.638182e-03 4.145400e-05 
#          499          500          501          502          503          504 
# 4.465959e-07 2.295813e-06 3.341313e-04 3.151623e-04 3.545844e-04 1.614989e-03 
#          505          506 
# 1.516662e-03 6.163582e-03 
# 
# $influence$influential
#   9  49 142 148 149 162 163 164 167 187 196 204 205 215 226 229 234 258 262 263 
#   9  49 142 148 149 162 163 164 167 187 196 204 205 215 226 229 234 258 262 263 
# 268 281 283 284 365 366 368 369 370 371 372 373 374 375 376 381 385 387 407 413 
# 268 281 283 284 365 366 368 369 370 371 372 373 374 375 376 381 385 387 407 413 
# 414 415 417 420 428 
# 414 415 417 420 428 
# 
# $influence$cutoff
# [1] 0.007968127
```

Remediation helpers operate on the fitted model directly.

``` r

fitWLS(model)
# 
# Call:
# lm(formula = medv ~ lstat + rm + crim, data = boston_housing)
# 
# Coefficients:
# (Intercept)        lstat           rm         crim  
#     -2.7307      -0.5748       5.2367      -0.1031
fitRobust(model)
# Call:
# rlm(formula = form, data = data)
# Converged in 7 iterations
# 
# Coefficients:
# (Intercept)       lstat          rm        crim 
#  -7.8028286  -0.5122876   5.8353526  -0.1386304 
# 
# Degrees of freedom: 506 total; 502 residual
# Scale estimate: 4.03
autoTransform(model)
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Breusch-Pagan.
# [INFO] Running Breusch-Pagan test
# [INFO] Running Breusch-Pagan test
# [INFO] Running Breusch-Pagan test
# [INFO] Running Breusch-Pagan test
# $model
# 
# Call:
# lm(formula = formula_trans, data = df)
# 
# Coefficients:
# (Intercept)        lstat           rm         crim  
#     -2.5623      -0.5785       5.2170      -0.1029  
# 
# 
# $method
# [1] "none"
# 
# $lambda
# [1] NA
```

See the package README for a complete list of implemented tests.

## Boston housing example

The `boston_housing` dataset contains median housing prices in Boston
suburbs along with socioeconomic predictors. We can run the diagnostics
and attempt a remedial fit on this real-world data.

``` r

data(boston_housing, package = "heteroTests")
boston_model <- lm(medv ~ lstat + rm + crim, data = boston_housing)
runDiagnostics(boston_model, boston_housing)
# $white
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
# 
# 
# $breusch_pagan
# 
#   Breusch-Pagan test for heteroscedasticity
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 24.344, df = 3, p-value = 2.117e-05
# 
# 
# $vif
#    lstat       rm     crim 
# 1.941883 1.616468 1.271372 
# 
# $reset
# 
#   RESET test for nonlinearity
# 
# data:  medv ~ lstat + rm + crim
# F = 133.3, df1 = 2, df2 = 500, p-value < 2.2e-16
# 
# 
# $influence
# $influence$cooks_distance
#            1            2            3            4            5            6 
# 8.844300e-04 3.516092e-04 2.175317e-04 6.563043e-05 9.082876e-04 2.143734e-05 
#            7            8            9           10           11           12 
# 3.967455e-05 3.666575e-03 8.786128e-03 7.949884e-08 1.095939e-03 1.122697e-04 
#           13           14           15           16           17           18 
# 2.009410e-04 4.416653e-04 6.289417e-04 4.438218e-04 1.097916e-04 1.603822e-04 
#           19           20           21           22           23           24 
# 7.842835e-05 2.519196e-04 2.018634e-05 1.735826e-05 4.576513e-04 1.267555e-04 
#           25           26           27           28           29           30 
# 2.948807e-04 3.658158e-04 1.802132e-04 5.251257e-04 6.487540e-04 4.488255e-04 
#           31           32           33           34           35           36 
# 1.231037e-04 9.551140e-04 1.177512e-04 4.276813e-04 7.573463e-04 5.289224e-04 
#           37           38           39           40           41           42 
# 5.556258e-05 1.609438e-04 1.266137e-04 8.843139e-05 2.152792e-04 4.087460e-04 
#           43           44           45           46           47           48 
# 4.218433e-05 2.250707e-05 1.610115e-04 1.701181e-04 1.023622e-05 8.421573e-05 
#           49           50           51           52           53           54 
# 8.160093e-03 1.770417e-04 2.732526e-05 3.145688e-04 4.135276e-04 7.653962e-06 
#           55           56           57           58           59           60 
# 1.316865e-05 4.258406e-04 2.725741e-04 3.294636e-05 1.976763e-04 4.410571e-04 
#           61           62           63           64           65           66 
# 3.815822e-05 4.366141e-04 7.481973e-04 1.235752e-04 4.001539e-04 8.220715e-04 
#           67           68           69           70           71           72 
# 2.190147e-04 9.651176e-05 1.146228e-04 1.978383e-04 2.437556e-04 3.964907e-05 
#           73           74           75           76           77           78 
# 5.346878e-04 1.576925e-04 1.591593e-04 3.203473e-04 2.065452e-04 1.760576e-04 
#           79           80           81           82           83           84 
# 5.121492e-05 2.641387e-04 7.368438e-05 3.954995e-04 9.023810e-05 1.954686e-04 
#           85           86           87           88           89           90 
# 3.493076e-05 7.747596e-05 2.955877e-05 1.656523e-04 1.958319e-03 2.229951e-04 
#           91           92           93           94           95           96 
# 2.304041e-04 4.104445e-04 2.807631e-04 6.460420e-05 2.261354e-04 1.899757e-06 
#           97           98           99          100          101          102 
# 5.505975e-05 3.389797e-04 5.672558e-03 2.633987e-05 4.539368e-06 9.353365e-05 
#          103          104          105          106          107          108 
# 7.118649e-04 1.174577e-04 1.134323e-04 3.675734e-05 2.432479e-04 1.571549e-05 
#          109          110          111          112          113          114 
# 3.964802e-04 6.243574e-05 5.650907e-06 3.497115e-04 2.726298e-07 1.235715e-05 
#          115          116          117          118          119          120 
# 6.272276e-04 2.428907e-05 4.428629e-05 3.776565e-04 4.324922e-05 7.308787e-07 
#          121          122          123          124          125          126 
# 1.401613e-04 9.474775e-07 2.052529e-04 1.804560e-03 2.706062e-05 4.361356e-05 
#          127          128          129          130          131          132 
# 2.949644e-03 3.476889e-05 5.212076e-04 1.385045e-04 4.538871e-04 2.357270e-04 
#          133          134          135          136          137          138 
# 2.615475e-05 1.343044e-05 1.040309e-04 2.366481e-04 4.735070e-05 8.663625e-04 
#          139          140          141          142          143          144 
# 3.329941e-04 4.152947e-05 2.968991e-04 2.788893e-02 1.342012e-03 2.673450e-03 
#          145          146          147          148          149          150 
# 5.188189e-03 7.930781e-05 6.122772e-05 1.144995e-02 1.252352e-02 1.095072e-04 
#          151          152          153          154          155          156 
# 3.962842e-06 1.819812e-04 1.479533e-04 7.023740e-05 2.729071e-04 4.485093e-04 
#          157          158          159          160          161          162 
# 2.877844e-04 4.370926e-03 4.224920e-05 3.534709e-04 3.351876e-06 1.605773e-02 
#          163          164          165          166          167          168 
# 1.710865e-02 2.114626e-02 8.496575e-05 7.709334e-05 2.041259e-02 2.305377e-04 
#          169          170          171          172          173          174 
# 5.155087e-08 5.176452e-05 1.228153e-04 8.968608e-05 1.068009e-03 9.304234e-05 
#          175          176          177          178          179          180 
# 1.309306e-06 2.919045e-05 1.292525e-06 1.668328e-04 1.409285e-05 1.509902e-03 
#          181          182          183          184          185          186 
# 3.959119e-03 3.914204e-03 1.587290e-03 5.681657e-04 2.456606e-03 1.237646e-03 
#          187          188          189          190          191          192 
# 2.028958e-02 2.716310e-04 2.683731e-05 4.363887e-04 1.469248e-03 1.462864e-05 
#          193          194          195          196          197          198 
# 5.415198e-04 4.306743e-05 2.668743e-06 1.796883e-02 2.368433e-06 2.441474e-05 
#          199          200          201          202          203          204 
# 4.630292e-04 5.542604e-04 3.015324e-05 5.001876e-05 3.745162e-03 1.529787e-02 
#          205          206          207          208          209          210 
# 1.857500e-02 1.717339e-05 2.083709e-06 1.105040e-03 3.500913e-04 5.141691e-03 
#          211          212          213          214          215          216 
# 3.385643e-04 4.913770e-03 4.891367e-04 1.719512e-04 3.709956e-02 1.635101e-05 
#          217          218          219          220          221          222 
# 2.362200e-04 1.104064e-04 4.278908e-04 4.995494e-05 6.093885e-05 1.501390e-03 
#          223          224          225          226          227          228 
# 8.295383e-09 1.673587e-04 7.139594e-03 2.450663e-02 5.705509e-07 1.136527e-05 
#          229          230          231          232          233          234 
# 1.096374e-02 2.095392e-04 1.517395e-04 1.040274e-04 8.488264e-04 1.616412e-02 
#          235          236          237          238          239          240 
# 3.455832e-05 3.066234e-05 4.001722e-05 1.327959e-04 4.650385e-04 4.805405e-04 
#          241          242          243          244          245          246 
# 8.347880e-04 8.193197e-05 6.983888e-05 6.854504e-04 1.395799e-04 2.820386e-04 
#          247          248          249          250          251          252 
# 3.152304e-06 2.709590e-04 1.950347e-05 1.757401e-04 4.014286e-04 9.019086e-04 
#          253          254          255          256          257          258 
# 1.948791e-04 2.988109e-03 5.776733e-04 1.382811e-04 5.906357e-03 2.649946e-02 
#          259          260          261          262          263          264 
# 1.323318e-03 3.124117e-05 9.660620e-04 8.170861e-03 2.378422e-02 2.607340e-04 
#          265          266          267          268          269          270 
# 1.789868e-03 3.574998e-04 1.840168e-03 3.506711e-02 5.401318e-03 2.066509e-06 
#          271          272          273          274          275          276 
# 1.233517e-05 3.486380e-05 1.744674e-04 1.902328e-04 1.374317e-04 1.395186e-05 
#          277          278          279          280          281          282 
# 9.134669e-05 2.452443e-04 1.101278e-04 8.994829e-04 8.496435e-03 7.270597e-04 
#          283          284          285          286          287          288 
# 8.737362e-03 1.854922e-02 2.108702e-04 4.459464e-04 1.137340e-04 2.204446e-04 
#          289          290          291          292          293          294 
# 3.919113e-04 4.044504e-05 3.574685e-04 1.058976e-03 7.554282e-05 8.670434e-06 
#          295          296          297          298          299          300 
# 3.178015e-05 3.887028e-08 1.280258e-06 1.057750e-04 1.196028e-03 2.416158e-04 
#          301          302          303          304          305          306 
# 8.024747e-04 4.013881e-04 2.356086e-07 1.659574e-04 1.151504e-03 5.853087e-05 
#          307          308          309          310          311          312 
# 6.126025e-05 1.064481e-05 1.714073e-03 1.947086e-04 1.013509e-05 6.841838e-04 
#          313          314          315          316          317          318 
# 1.683386e-04 4.462414e-04 1.310739e-04 7.622177e-04 8.193619e-07 6.587399e-05 
#          319          320          321          322          323          324 
# 4.925570e-05 1.719524e-05 2.491439e-04 4.029244e-04 6.672045e-04 1.421605e-04 
#          325          326          327          328          329          330 
# 1.858561e-04 4.795729e-04 5.370169e-04 4.362190e-06 3.319916e-04 3.898823e-04 
#          331          332          333          334          335          336 
# 5.455701e-04 3.182469e-04 9.909742e-04 9.763435e-04 1.115058e-03 3.942553e-04 
#          337          338          339          340          341          342 
# 3.185216e-04 4.257727e-04 4.271210e-04 5.173927e-04 7.080802e-04 2.119588e-05 
#          343          344          345          346          347          348 
# 2.246889e-03 4.968805e-04 1.248104e-05 7.511594e-04 3.670878e-04 6.608075e-04 
#          349          350          351          352          353          354 
# 5.187241e-04 4.560976e-04 8.050246e-04 6.874789e-04 1.269240e-03 1.081551e-06 
#          355          356          357          358          359          360 
# 1.146015e-03 1.385284e-03 2.542854e-05 1.876568e-05 4.841101e-06 1.990030e-05 
#          361          362          363          364          365          366 
# 2.130953e-05 4.190971e-05 2.302168e-04 9.999044e-05 8.866643e-02 1.602101e-01 
#          367          368          369          370          371          372 
# 5.055707e-03 8.458902e-02 2.287920e-01 2.431819e-02 2.263697e-02 2.823846e-02 
#          373          374          375          376          377          378 
# 4.617108e-02 2.720090e-02 9.006181e-02 1.638607e-02 1.189021e-03 3.903946e-03 
#          379          380          381          382          383          384 
# 3.066173e-04 2.180138e-03 6.126804e-02 4.009264e-03 9.950492e-06 7.722725e-05 
#          385          386          387          388          389          390 
# 1.266343e-02 4.137364e-04 1.029602e-02 3.642113e-03 5.488101e-03 6.309435e-05 
#          391          392          393          394          395          396 
# 6.608891e-05 9.554045e-04 3.513101e-04 8.822601e-04 8.639494e-04 1.889009e-03 
#          397          398          399          400          401          402 
# 2.030117e-03 1.493381e-03 1.817111e-04 1.556224e-03 3.764478e-03 6.150218e-03 
#          403          404          405          406          407          408 
# 1.964810e-03 1.317068e-03 1.965182e-03 4.300851e-03 1.691781e-02 6.228249e-03 
#          409          410          411          412          413          414 
# 3.573677e-03 5.111843e-03 1.651627e-03 1.401685e-04 6.133840e-02 8.628304e-03 
#          415          416          417          418          419          420 
# 7.375963e-02 4.880727e-03 1.448958e-02 1.676176e-03 8.116982e-05 1.303394e-02 
#          421          422          423          424          425          426 
# 6.063831e-04 4.754423e-04 5.778351e-04 1.906904e-04 5.724495e-04 1.310566e-03 
#          427          428          429          430          431          432 
# 2.076019e-03 1.540037e-02 1.647050e-03 3.770956e-03 8.068005e-04 3.666093e-03 
#          433          434          435          436          437          438 
# 1.036434e-03 1.274290e-03 2.578912e-03 1.726739e-03 5.069375e-03 1.725791e-03 
#          439          440          441          442          443          444 
# 2.653510e-04 2.216424e-06 4.802200e-04 1.036050e-04 3.917928e-05 7.511803e-04 
#          445          446          447          448          449          450 
# 2.754776e-04 1.892377e-03 7.099893e-04 1.327721e-03 5.322785e-04 1.656114e-03 
#          451          452          453          454          455          456 
# 3.931962e-03 1.909585e-03 3.853354e-04 7.275801e-03 2.209546e-03 1.876754e-03 
#          457          458          459          460          461          462 
# 6.250209e-04 4.438266e-04 6.700703e-04 3.407380e-08 1.596854e-03 3.547041e-04 
#          463          464          465          466          467          468 
# 8.533945e-05 4.778956e-04 1.294172e-08 2.323751e-05 1.660639e-05 4.845874e-04 
#          469          470          471          472          473          474 
# 3.986063e-04 3.835791e-04 5.632020e-07 1.016027e-04 1.564403e-05 3.904395e-04 
#          475          476          477          478          479          480 
# 1.671795e-05 2.339445e-04 5.056293e-04 6.182399e-04 4.185972e-04 1.329940e-05 
#          481          482          483          484          485          486 
# 8.040202e-07 4.817225e-04 9.314928e-04 1.611772e-05 6.004931e-06 1.343352e-04 
#          487          488          489          490          491          492 
# 1.782949e-05 8.090930e-06 2.577414e-06 1.986397e-03 2.662820e-04 8.035713e-04 
#          493          494          495          496          497          498 
# 1.593268e-05 9.362844e-05 4.096794e-04 1.554364e-03 2.638182e-03 4.145400e-05 
#          499          500          501          502          503          504 
# 4.465959e-07 2.295813e-06 3.341313e-04 3.151623e-04 3.545844e-04 1.614989e-03 
#          505          506 
# 1.516662e-03 6.163582e-03 
# 
# $influence$influential
#   9  49 142 148 149 162 163 164 167 187 196 204 205 215 226 229 234 258 262 263 
#   9  49 142 148 149 162 163 164 167 187 196 204 205 215 226 229 234 258 262 263 
# 268 281 283 284 365 366 368 369 370 371 372 373 374 375 376 381 385 387 407 413 
# 268 281 283 284 365 366 368 369 370 371 372 373 374 375 376 381 385 387 407 413 
# 414 415 417 420 428 
# 414 415 417 420 428 
# 
# $influence$cutoff
# [1] 0.007968127
fitWLS(boston_model)
# 
# Call:
# lm(formula = medv ~ lstat + rm + crim, data = boston_housing)
# 
# Coefficients:
# (Intercept)        lstat           rm         crim  
#     -2.7307      -0.5748       5.2367      -0.1031
```

## Theophylline dosing example

The `Theoph` dataset from base R contains blood concentration measures
after a single dose. Diagnostics reveal heteroscedasticity and let us
visualise the residual spread.

``` r

data(Theoph, package = "datasets")
dose_model <- lm(conc ~ Time, data = Theoph)
performWhiteTest(dose_model, Theoph)
# [INFO] Running White test
# [INFO] White test completed: statistic = 40.4104 df = 2 p = 0
# 
#   White's test for heteroscedasticity
# 
# data:  dose_model
# X-squared = 40.41, df = 2, p-value = 1.679e-09
# alternative hypothesis: heteroscedasticity present
plot(HeteroDiagnostic(dose_model, Theoph))
# $residuals_fitted
# `geom_smooth()` using formula = 'y ~ x'
```

![](using_heteroTests_files/figure-html/theoph-example-1.png)

    # 
    # $spread_level
    # `geom_smooth()` using formula = 'y ~ x'

![](using_heteroTests_files/figure-html/theoph-example-2.png)

    # 
    # $density

![](using_heteroTests_files/figure-html/theoph-example-3.png)

    # 
    # $qq

![](using_heteroTests_files/figure-html/theoph-example-4.png)

    # 
    # $bubble_variance

![](using_heteroTests_files/figure-html/theoph-example-5.png)

## Step-by-step workflow

This short example shows how to detect heteroscedasticity, choose a
remedy and re-test the model.

``` r

hd <- HeteroDiagnostic(medv ~ lstat + rm + crim, boston_housing)
test(hd)
# $white
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
# 
# 
# $breusch_pagan
# 
#   Breusch-Pagan test for heteroscedasticity
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 24.344, df = 3, p-value = 2.117e-05
# 
# 
# $vif
#    lstat       rm     crim 
# 1.941883 1.616468 1.271372 
# 
# $reset
# 
#   RESET test for nonlinearity
# 
# data:  medv ~ lstat + rm + crim
# F = 133.3, df1 = 2, df2 = 500, p-value < 2.2e-16
# 
# 
# $influence
# $influence$cooks_distance
#            1            2            3            4            5            6 
# 8.844300e-04 3.516092e-04 2.175317e-04 6.563043e-05 9.082876e-04 2.143734e-05 
#            7            8            9           10           11           12 
# 3.967455e-05 3.666575e-03 8.786128e-03 7.949884e-08 1.095939e-03 1.122697e-04 
#           13           14           15           16           17           18 
# 2.009410e-04 4.416653e-04 6.289417e-04 4.438218e-04 1.097916e-04 1.603822e-04 
#           19           20           21           22           23           24 
# 7.842835e-05 2.519196e-04 2.018634e-05 1.735826e-05 4.576513e-04 1.267555e-04 
#           25           26           27           28           29           30 
# 2.948807e-04 3.658158e-04 1.802132e-04 5.251257e-04 6.487540e-04 4.488255e-04 
#           31           32           33           34           35           36 
# 1.231037e-04 9.551140e-04 1.177512e-04 4.276813e-04 7.573463e-04 5.289224e-04 
#           37           38           39           40           41           42 
# 5.556258e-05 1.609438e-04 1.266137e-04 8.843139e-05 2.152792e-04 4.087460e-04 
#           43           44           45           46           47           48 
# 4.218433e-05 2.250707e-05 1.610115e-04 1.701181e-04 1.023622e-05 8.421573e-05 
#           49           50           51           52           53           54 
# 8.160093e-03 1.770417e-04 2.732526e-05 3.145688e-04 4.135276e-04 7.653962e-06 
#           55           56           57           58           59           60 
# 1.316865e-05 4.258406e-04 2.725741e-04 3.294636e-05 1.976763e-04 4.410571e-04 
#           61           62           63           64           65           66 
# 3.815822e-05 4.366141e-04 7.481973e-04 1.235752e-04 4.001539e-04 8.220715e-04 
#           67           68           69           70           71           72 
# 2.190147e-04 9.651176e-05 1.146228e-04 1.978383e-04 2.437556e-04 3.964907e-05 
#           73           74           75           76           77           78 
# 5.346878e-04 1.576925e-04 1.591593e-04 3.203473e-04 2.065452e-04 1.760576e-04 
#           79           80           81           82           83           84 
# 5.121492e-05 2.641387e-04 7.368438e-05 3.954995e-04 9.023810e-05 1.954686e-04 
#           85           86           87           88           89           90 
# 3.493076e-05 7.747596e-05 2.955877e-05 1.656523e-04 1.958319e-03 2.229951e-04 
#           91           92           93           94           95           96 
# 2.304041e-04 4.104445e-04 2.807631e-04 6.460420e-05 2.261354e-04 1.899757e-06 
#           97           98           99          100          101          102 
# 5.505975e-05 3.389797e-04 5.672558e-03 2.633987e-05 4.539368e-06 9.353365e-05 
#          103          104          105          106          107          108 
# 7.118649e-04 1.174577e-04 1.134323e-04 3.675734e-05 2.432479e-04 1.571549e-05 
#          109          110          111          112          113          114 
# 3.964802e-04 6.243574e-05 5.650907e-06 3.497115e-04 2.726298e-07 1.235715e-05 
#          115          116          117          118          119          120 
# 6.272276e-04 2.428907e-05 4.428629e-05 3.776565e-04 4.324922e-05 7.308787e-07 
#          121          122          123          124          125          126 
# 1.401613e-04 9.474775e-07 2.052529e-04 1.804560e-03 2.706062e-05 4.361356e-05 
#          127          128          129          130          131          132 
# 2.949644e-03 3.476889e-05 5.212076e-04 1.385045e-04 4.538871e-04 2.357270e-04 
#          133          134          135          136          137          138 
# 2.615475e-05 1.343044e-05 1.040309e-04 2.366481e-04 4.735070e-05 8.663625e-04 
#          139          140          141          142          143          144 
# 3.329941e-04 4.152947e-05 2.968991e-04 2.788893e-02 1.342012e-03 2.673450e-03 
#          145          146          147          148          149          150 
# 5.188189e-03 7.930781e-05 6.122772e-05 1.144995e-02 1.252352e-02 1.095072e-04 
#          151          152          153          154          155          156 
# 3.962842e-06 1.819812e-04 1.479533e-04 7.023740e-05 2.729071e-04 4.485093e-04 
#          157          158          159          160          161          162 
# 2.877844e-04 4.370926e-03 4.224920e-05 3.534709e-04 3.351876e-06 1.605773e-02 
#          163          164          165          166          167          168 
# 1.710865e-02 2.114626e-02 8.496575e-05 7.709334e-05 2.041259e-02 2.305377e-04 
#          169          170          171          172          173          174 
# 5.155087e-08 5.176452e-05 1.228153e-04 8.968608e-05 1.068009e-03 9.304234e-05 
#          175          176          177          178          179          180 
# 1.309306e-06 2.919045e-05 1.292525e-06 1.668328e-04 1.409285e-05 1.509902e-03 
#          181          182          183          184          185          186 
# 3.959119e-03 3.914204e-03 1.587290e-03 5.681657e-04 2.456606e-03 1.237646e-03 
#          187          188          189          190          191          192 
# 2.028958e-02 2.716310e-04 2.683731e-05 4.363887e-04 1.469248e-03 1.462864e-05 
#          193          194          195          196          197          198 
# 5.415198e-04 4.306743e-05 2.668743e-06 1.796883e-02 2.368433e-06 2.441474e-05 
#          199          200          201          202          203          204 
# 4.630292e-04 5.542604e-04 3.015324e-05 5.001876e-05 3.745162e-03 1.529787e-02 
#          205          206          207          208          209          210 
# 1.857500e-02 1.717339e-05 2.083709e-06 1.105040e-03 3.500913e-04 5.141691e-03 
#          211          212          213          214          215          216 
# 3.385643e-04 4.913770e-03 4.891367e-04 1.719512e-04 3.709956e-02 1.635101e-05 
#          217          218          219          220          221          222 
# 2.362200e-04 1.104064e-04 4.278908e-04 4.995494e-05 6.093885e-05 1.501390e-03 
#          223          224          225          226          227          228 
# 8.295383e-09 1.673587e-04 7.139594e-03 2.450663e-02 5.705509e-07 1.136527e-05 
#          229          230          231          232          233          234 
# 1.096374e-02 2.095392e-04 1.517395e-04 1.040274e-04 8.488264e-04 1.616412e-02 
#          235          236          237          238          239          240 
# 3.455832e-05 3.066234e-05 4.001722e-05 1.327959e-04 4.650385e-04 4.805405e-04 
#          241          242          243          244          245          246 
# 8.347880e-04 8.193197e-05 6.983888e-05 6.854504e-04 1.395799e-04 2.820386e-04 
#          247          248          249          250          251          252 
# 3.152304e-06 2.709590e-04 1.950347e-05 1.757401e-04 4.014286e-04 9.019086e-04 
#          253          254          255          256          257          258 
# 1.948791e-04 2.988109e-03 5.776733e-04 1.382811e-04 5.906357e-03 2.649946e-02 
#          259          260          261          262          263          264 
# 1.323318e-03 3.124117e-05 9.660620e-04 8.170861e-03 2.378422e-02 2.607340e-04 
#          265          266          267          268          269          270 
# 1.789868e-03 3.574998e-04 1.840168e-03 3.506711e-02 5.401318e-03 2.066509e-06 
#          271          272          273          274          275          276 
# 1.233517e-05 3.486380e-05 1.744674e-04 1.902328e-04 1.374317e-04 1.395186e-05 
#          277          278          279          280          281          282 
# 9.134669e-05 2.452443e-04 1.101278e-04 8.994829e-04 8.496435e-03 7.270597e-04 
#          283          284          285          286          287          288 
# 8.737362e-03 1.854922e-02 2.108702e-04 4.459464e-04 1.137340e-04 2.204446e-04 
#          289          290          291          292          293          294 
# 3.919113e-04 4.044504e-05 3.574685e-04 1.058976e-03 7.554282e-05 8.670434e-06 
#          295          296          297          298          299          300 
# 3.178015e-05 3.887028e-08 1.280258e-06 1.057750e-04 1.196028e-03 2.416158e-04 
#          301          302          303          304          305          306 
# 8.024747e-04 4.013881e-04 2.356086e-07 1.659574e-04 1.151504e-03 5.853087e-05 
#          307          308          309          310          311          312 
# 6.126025e-05 1.064481e-05 1.714073e-03 1.947086e-04 1.013509e-05 6.841838e-04 
#          313          314          315          316          317          318 
# 1.683386e-04 4.462414e-04 1.310739e-04 7.622177e-04 8.193619e-07 6.587399e-05 
#          319          320          321          322          323          324 
# 4.925570e-05 1.719524e-05 2.491439e-04 4.029244e-04 6.672045e-04 1.421605e-04 
#          325          326          327          328          329          330 
# 1.858561e-04 4.795729e-04 5.370169e-04 4.362190e-06 3.319916e-04 3.898823e-04 
#          331          332          333          334          335          336 
# 5.455701e-04 3.182469e-04 9.909742e-04 9.763435e-04 1.115058e-03 3.942553e-04 
#          337          338          339          340          341          342 
# 3.185216e-04 4.257727e-04 4.271210e-04 5.173927e-04 7.080802e-04 2.119588e-05 
#          343          344          345          346          347          348 
# 2.246889e-03 4.968805e-04 1.248104e-05 7.511594e-04 3.670878e-04 6.608075e-04 
#          349          350          351          352          353          354 
# 5.187241e-04 4.560976e-04 8.050246e-04 6.874789e-04 1.269240e-03 1.081551e-06 
#          355          356          357          358          359          360 
# 1.146015e-03 1.385284e-03 2.542854e-05 1.876568e-05 4.841101e-06 1.990030e-05 
#          361          362          363          364          365          366 
# 2.130953e-05 4.190971e-05 2.302168e-04 9.999044e-05 8.866643e-02 1.602101e-01 
#          367          368          369          370          371          372 
# 5.055707e-03 8.458902e-02 2.287920e-01 2.431819e-02 2.263697e-02 2.823846e-02 
#          373          374          375          376          377          378 
# 4.617108e-02 2.720090e-02 9.006181e-02 1.638607e-02 1.189021e-03 3.903946e-03 
#          379          380          381          382          383          384 
# 3.066173e-04 2.180138e-03 6.126804e-02 4.009264e-03 9.950492e-06 7.722725e-05 
#          385          386          387          388          389          390 
# 1.266343e-02 4.137364e-04 1.029602e-02 3.642113e-03 5.488101e-03 6.309435e-05 
#          391          392          393          394          395          396 
# 6.608891e-05 9.554045e-04 3.513101e-04 8.822601e-04 8.639494e-04 1.889009e-03 
#          397          398          399          400          401          402 
# 2.030117e-03 1.493381e-03 1.817111e-04 1.556224e-03 3.764478e-03 6.150218e-03 
#          403          404          405          406          407          408 
# 1.964810e-03 1.317068e-03 1.965182e-03 4.300851e-03 1.691781e-02 6.228249e-03 
#          409          410          411          412          413          414 
# 3.573677e-03 5.111843e-03 1.651627e-03 1.401685e-04 6.133840e-02 8.628304e-03 
#          415          416          417          418          419          420 
# 7.375963e-02 4.880727e-03 1.448958e-02 1.676176e-03 8.116982e-05 1.303394e-02 
#          421          422          423          424          425          426 
# 6.063831e-04 4.754423e-04 5.778351e-04 1.906904e-04 5.724495e-04 1.310566e-03 
#          427          428          429          430          431          432 
# 2.076019e-03 1.540037e-02 1.647050e-03 3.770956e-03 8.068005e-04 3.666093e-03 
#          433          434          435          436          437          438 
# 1.036434e-03 1.274290e-03 2.578912e-03 1.726739e-03 5.069375e-03 1.725791e-03 
#          439          440          441          442          443          444 
# 2.653510e-04 2.216424e-06 4.802200e-04 1.036050e-04 3.917928e-05 7.511803e-04 
#          445          446          447          448          449          450 
# 2.754776e-04 1.892377e-03 7.099893e-04 1.327721e-03 5.322785e-04 1.656114e-03 
#          451          452          453          454          455          456 
# 3.931962e-03 1.909585e-03 3.853354e-04 7.275801e-03 2.209546e-03 1.876754e-03 
#          457          458          459          460          461          462 
# 6.250209e-04 4.438266e-04 6.700703e-04 3.407380e-08 1.596854e-03 3.547041e-04 
#          463          464          465          466          467          468 
# 8.533945e-05 4.778956e-04 1.294172e-08 2.323751e-05 1.660639e-05 4.845874e-04 
#          469          470          471          472          473          474 
# 3.986063e-04 3.835791e-04 5.632020e-07 1.016027e-04 1.564403e-05 3.904395e-04 
#          475          476          477          478          479          480 
# 1.671795e-05 2.339445e-04 5.056293e-04 6.182399e-04 4.185972e-04 1.329940e-05 
#          481          482          483          484          485          486 
# 8.040202e-07 4.817225e-04 9.314928e-04 1.611772e-05 6.004931e-06 1.343352e-04 
#          487          488          489          490          491          492 
# 1.782949e-05 8.090930e-06 2.577414e-06 1.986397e-03 2.662820e-04 8.035713e-04 
#          493          494          495          496          497          498 
# 1.593268e-05 9.362844e-05 4.096794e-04 1.554364e-03 2.638182e-03 4.145400e-05 
#          499          500          501          502          503          504 
# 4.465959e-07 2.295813e-06 3.341313e-04 3.151623e-04 3.545844e-04 1.614989e-03 
#          505          506 
# 1.516662e-03 6.163582e-03 
# 
# $influence$influential
#   9  49 142 148 149 162 163 164 167 187 196 204 205 215 226 229 234 258 262 263 
#   9  49 142 148 149 162 163 164 167 187 196 204 205 215 226 229 234 258 262 263 
# 268 281 283 284 365 366 368 369 370 371 372 373 374 375 376 381 385 387 407 413 
# 268 281 283 284 365 366 368 369 370 371 372 373 374 375 376 381 385 387 407 413 
# 414 415 417 420 428 
# 414 415 417 420 428 
# 
# $influence$cutoff
# [1] 0.007968127

wls_model <- fitWLS(hd$model)

# Re-test after remedy
test(HeteroDiagnostic(wls_model, boston_housing))
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running White.
# [INFO] Running White test
# [INFO] White test completed: statistic = 135.1477 df = 9 p = 0
# Warning: Residual outliers detected at rows 369, 373 (|z| > 5). Inspect
# leverage before running Breusch-Pagan.
# [INFO] Running Breusch-Pagan test
# $white
# 
#   White's test for heteroscedasticity
# 
# data:  model
# X-squared = 135.15, df = 9, p-value < 2.2e-16
# alternative hypothesis: heteroscedasticity present
# 
# 
# $breusch_pagan
# 
#   Breusch-Pagan test for heteroscedasticity
# 
# data:  medv ~ lstat + rm + crim
# X-squared = 24.622, df = 3, p-value = 1.852e-05
# 
# 
# $vif
#    lstat       rm     crim 
# 1.941883 1.616468 1.271372 
# 
# $reset
# 
#   RESET test for nonlinearity
# 
# data:  medv ~ lstat + rm + crim
# F = 133.33, df1 = 2, df2 = 500, p-value < 2.2e-16
# 
# 
# $influence
# $influence$cooks_distance
#            1            2            3            4            5            6 
# 1.253861e-05 5.734216e-06 3.953073e-05 2.258298e-04 5.760945e-06 7.131288e-04 
#            7            8            9           10           11           12 
# 7.587255e-05 5.223898e-06 3.550350e-05 9.686541e-02 4.380757e-05 2.371676e-05 
#           13           14           15           16           17           18 
# 2.218069e-05 3.765834e-05 6.448146e-06 5.109695e-05 2.962191e-04 1.616943e-05 
#           19           20           21           22           23           24 
# 3.922970e-04 4.950188e-05 1.155266e-03 1.495698e-04 2.857434e-05 1.257056e-04 
#           25           26           27           28           29           30 
# 1.471702e-05 2.315705e-05 2.428983e-05 1.166328e-05 1.925239e-06 3.175024e-06 
#           31           32           33           34           35           36 
# 3.256373e-04 1.779798e-06 1.734978e-03 2.165319e-05 3.116002e-05 1.942815e-05 
#           37           38           39           40           41           42 
# 1.421802e-04 1.199214e-04 6.081108e-05 1.698434e-04 9.821927e-05 1.967562e-05 
#           43           44           45           46           47           48 
# 5.126340e-04 4.169950e-04 3.929512e-05 1.243216e-04 5.310674e-04 1.634578e-04 
#           49           50           51           52           53           54 
# 3.976102e-05 5.139238e-05 1.133222e-04 1.780949e-05 2.698940e-05 1.626587e-03 
#           55           56           57           58           59           60 
# 3.060076e-04 1.613301e-05 4.535515e-05 3.640336e-04 7.803706e-05 2.836001e-05 
#           61           62           63           64           65           66 
# 1.937286e-04 6.997685e-06 8.483989e-06 8.142947e-06 7.345155e-06 3.125437e-05 
#           67           68           69           70           71           72 
# 6.615345e-05 2.305753e-04 1.145653e-04 8.591040e-05 2.917989e-05 2.104349e-04 
#           73           74           75           76           77           78 
# 6.276186e-05 5.198086e-05 6.708947e-05 1.129272e-05 6.038802e-06 1.996836e-05 
#           79           80           81           82           83           84 
# 2.751379e-05 5.985850e-05 9.302097e-05 7.994548e-06 1.106349e-04 5.549652e-05 
#           85           86           87           88           89           90 
# 5.053977e-05 5.717635e-05 9.384940e-05 5.085849e-05 2.361841e-06 1.932820e-05 
#           91           92           93           94           95           96 
# 1.047909e-05 8.443398e-06 1.114875e-05 2.536132e-04 8.810407e-06 2.525238e-03 
#           97           98           99          100          101          102 
# 3.877882e-05 1.578074e-04 4.766235e-06 3.192678e-04 2.144246e-04 1.974824e-05 
#          103          104          105          106          107          108 
# 1.516672e-06 1.534959e-05 1.485289e-05 1.508227e-04 4.618232e-05 1.353721e-04 
#          109          110          111          112          113          114 
# 2.774763e-06 6.033865e-05 2.774478e-04 2.693343e-06 2.422217e-02 5.703160e-04 
#          115          116          117          118          119          120 
# 3.248318e-06 1.748539e-04 3.848957e-05 1.497263e-05 9.939693e-05 9.807270e-03 
#          121          122          123          124          125          126 
# 2.919268e-05 3.095370e-03 4.325716e-05 6.428903e-05 2.807661e-04 7.017690e-05 
#          127          128          129          130          131          132 
# 5.242890e-05 2.401031e-04 9.970909e-06 7.529706e-05 2.567121e-06 3.584542e-06 
#          133          134          135          136          137          138 
# 3.681358e-05 3.422371e-04 6.507060e-05 3.835902e-05 1.233693e-04 4.034216e-06 
#          139          140          141          142          143          144 
# 9.353687e-05 3.243016e-04 4.200175e-04 1.892959e-05 7.044105e-05 3.095159e-05 
#          145          146          147          148          149          150 
# 3.206913e-05 2.970197e-03 1.073454e-04 1.577688e-05 1.130639e-05 1.839641e-04 
#          151          152          153          154          155          156 
# 3.647767e-04 1.289617e-04 6.528285e-04 6.988072e-05 7.972444e-06 3.213805e-06 
#          157          158          159          160          161          162 
# 7.044927e-05 1.769529e-06 5.482177e-04 1.086512e-05 7.908342e-03 1.415443e-06 
#          163          164          165          166          167          168 
# 1.906788e-06 5.257468e-06 7.334925e-05 6.225655e-05 1.911625e-06 2.067764e-05 
#          169          170          171          172          173          174 
# 2.361986e-02 1.226229e-05 2.652162e-05 5.224524e-05 1.018659e-05 2.296425e-05 
#          175          176          177          178          179          180 
# 1.388518e-02 3.631519e-04 5.591001e-03 7.058343e-05 1.840474e-04 3.829787e-06 
#          181          182          183          184          185          186 
# 8.847466e-06 1.274755e-06 3.975790e-06 1.454092e-05 4.404354e-06 1.418643e-06 
#          187          188          189          190          191          192 
# 1.394390e-06 1.151547e-05 5.624729e-04 1.226275e-05 3.905487e-06 6.692158e-04 
#          193          194          195          196          197          198 
# 2.471071e-05 1.682586e-04 4.538618e-03 1.738650e-06 4.215452e-03 1.313049e-04 
#          199          200          201          202          203          204 
# 1.134730e-05 1.306349e-05 2.446628e-04 2.250376e-04 4.694893e-06 1.921488e-06 
#          205          206          207          208          209          210 
# 2.460245e-06 4.732917e-04 6.211813e-04 8.346445e-06 7.518752e-06 8.875693e-06 
#          211          212          213          214          215          216 
# 1.991229e-05 1.170885e-05 1.103431e-05 1.221379e-05 6.625685e-06 2.649965e-04 
#          217          218          219          220          221          222 
# 1.725285e-05 8.340474e-06 2.059032e-05 2.556923e-05 3.365945e-05 3.044119e-05 
#          223          224          225          226          227          228 
# 3.816534e+00 1.532820e-05 1.249957e-05 1.104047e-05 1.757725e-01 3.596429e-04 
#          229          230          231          232          233          234 
# 1.758129e-06 1.014181e-04 2.621561e-05 8.371536e-05 1.098246e-04 5.173550e-06 
#          235          236          237          238          239          240 
# 4.699965e-05 1.122564e-04 2.246452e-05 6.309580e-05 1.532470e-05 6.249304e-06 
#          241          242          243          244          245          246 
# 4.067900e-06 2.647072e-05 1.574125e-05 2.300095e-05 1.079399e-04 4.360849e-05 
#          247          248          249          250          251          252 
# 2.256684e-03 9.487841e-06 8.004561e-05 2.075481e-05 2.194320e-05 3.109779e-05 
#          253          254          255          256          257          258 
# 5.850580e-05 2.776972e-05 3.407896e-05 1.070567e-04 2.349410e-06 1.037963e-05 
#          259          260          261          262          263          264 
# 5.501888e-06 8.438043e-05 6.820305e-06 1.701140e-06 6.501680e-06 7.533010e-05 
#          265          266          267          268          269          270 
# 2.502478e-06 8.497914e-05 1.352213e-05 4.385884e-06 2.663200e-06 1.758813e-03 
#          271          272          273          274          275          276 
# 3.962246e-04 3.617494e-04 1.683525e-05 1.190042e-04 1.136583e-04 1.311355e-03 
#          277          278          279          280          281          282 
# 5.929354e-05 4.248297e-05 4.295401e-05 8.482731e-06 3.172489e-06 9.868828e-06 
#          283          284          285          286          287          288 
# 2.181390e-06 1.883895e-06 1.301684e-05 6.547971e-06 1.298497e-05 5.056722e-05 
#          289          290          291          292          293          294 
# 1.620783e-05 2.709313e-05 3.913435e-05 9.682796e-06 1.457885e-04 8.579558e-04 
#          295          296          297          298          299          300 
# 1.781974e-04 1.757319e-01 2.480310e-03 5.216160e-05 1.648209e-05 2.627082e-05 
#          301          302          303          304          305          306 
# 4.745670e-06 2.617384e-06 1.071824e-02 3.815345e-05 3.997001e-06 2.201356e-05 
#          307          308          309          310          311          312 
# 1.406093e-04 1.769344e-04 7.098494e-06 3.970071e-05 1.221030e-02 3.534090e-05 
#          313          314          315          316          317          318 
# 1.977049e-05 1.444697e-05 8.829838e-06 1.717420e-05 1.068609e-02 8.458566e-05 
#          319          320          321          322          323          324 
# 2.519791e-05 1.046429e-04 2.176801e-05 1.856449e-05 2.297763e-05 8.400299e-05 
#          325          326          327          328          329          330 
# 5.159817e-05 3.128422e-05 2.380984e-05 4.845931e-04 3.548732e-05 1.744156e-05 
#          331          332          333          334          335          336 
# 1.067864e-05 3.203091e-05 1.526565e-05 1.601089e-05 8.753356e-06 3.476460e-05 
#          337          338          339          340          341          342 
# 3.924504e-05 2.022541e-05 2.412867e-05 1.588048e-05 1.481450e-05 2.706879e-04 
#          343          344          345          346          347          348 
# 7.967957e-07 5.508721e-06 6.607359e-04 7.172296e-06 1.222607e-05 9.829817e-06 
#          349          350          351          352          353          354 
# 1.131026e-05 8.659575e-06 1.046421e-05 1.249678e-05 1.967128e-05 1.271073e-02 
#          355          356          357          358          359          360 
# 3.945315e-05 3.404114e-05 2.426846e-04 4.272006e-05 4.414776e-04 6.207279e-05 
#          361          362          363          364          365          366 
# 2.527580e-04 2.410018e-05 2.693172e-04 3.060486e-05 3.885814e-06 1.916955e-05 
#          367          368          369          370          371          372 
# 1.498464e-05 1.215727e-05 3.166246e-06 1.040984e-06 1.181347e-06 3.079914e-07 
#          373          374          375          376          377          378 
# 5.716333e-07 1.323336e-05 5.921183e-06 7.864519e-06 1.305767e-04 2.685133e-05 
#          379          380          381          382          383          384 
# 6.540515e-04 2.737045e-05 6.542288e-04 1.869791e-05 3.243329e-03 4.790939e-04 
#          385          386          387          388          389          390 
# 1.547581e-05 4.115318e-04 1.679476e-05 6.563647e-05 2.654863e-05 2.251885e-04 
#          391          392          393          394          395          396 
# 6.184796e-05 7.538955e-06 1.425255e-04 2.408389e-06 8.821364e-06 5.127705e-06 
#          397          398          399          400          401          402 
# 1.044218e-05 5.730265e-06 3.806237e-03 1.641885e-04 6.092189e-05 5.691848e-06 
#          403          404          405          406          407          408 
# 1.556198e-05 7.217972e-05 4.175080e-04 1.860694e-03 1.441519e-05 4.587442e-06 
#          409          410          411          412          413          414 
# 2.110896e-05 1.798101e-05 2.131529e-03 6.582101e-04 4.927769e-06 2.164211e-05 
#          415          416          417          418          419          420 
# 2.187724e-05 8.976274e-05 2.306227e-05 9.039235e-05 1.106503e+00 1.312868e-05 
#          421          422          423          424          425          426 
# 8.688297e-06 3.994151e-06 2.437262e-05 2.772681e-04 1.163504e-05 4.707682e-05 
#          427          428          429          430          431          432 
# 3.120096e-06 4.044011e-05 1.857238e-05 2.786744e-05 1.036166e-05 1.983236e-05 
#          433          434          435          436          437          438 
# 9.039544e-07 3.908438e-06 3.148661e-06 7.819107e-05 4.772511e-06 8.689389e-05 
#          439          440          441          442          443          444 
# 2.151931e-03 8.748730e-03 1.432030e-04 2.269605e-04 8.340730e-05 2.935097e-05 
#          445          446          447          448          449          450 
# 1.542866e-04 6.261598e-05 1.181056e-05 3.426631e-06 1.372498e-05 1.250639e-05 
#          451          452          453          454          455          456 
# 6.413568e-06 1.129178e-05 1.516514e-05 1.714798e-05 1.805598e-05 9.476552e-06 
#          457          458          459          460          461          462 
# 1.244126e-05 6.983513e-06 5.318064e-06 4.704712e-02 8.931867e-06 4.827801e-06 
#          463          464          465          466          467          468 
# 1.387419e-05 2.621717e-06 3.195351e-01 1.841383e-04 2.066661e-04 4.656419e-05 
#          469          470          471          472          473          474 
# 3.474881e-05 3.125568e-05 4.063001e-03 7.478656e-06 1.096381e-04 1.330936e-05 
#          475          476          477          478          479          480 
# 5.695712e-04 3.420876e-04 4.019014e-05 7.103907e-05 1.825011e-05 7.634034e-04 
#          481          482          483          484          485          486 
# 2.490455e-03 7.277232e-06 6.013440e-06 9.436327e-04 5.565970e-04 9.558736e-06 
#          487          488          489          490          491          492 
# 7.444902e-05 6.601652e-04 6.460889e-03 3.007462e-05 7.998988e-04 1.205150e-05 
#          493          494          495          496          497          498 
# 1.818182e-04 1.229214e-04 8.225056e-06 6.029640e-06 1.067403e-05 1.244926e-04 
#          499          500          501          502          503          504 
# 5.913522e-03 4.607682e-03 7.719781e-06 3.100253e-06 1.807232e-05 2.710533e-06 
#          505          506 
# 2.230985e-06 2.424121e-06 
# 
# $influence$influential
#  10 113 120 169 175 223 227 296 303 311 317 354 419 440 460 465 
#  10 113 120 169 175 223 227 296 303 311 317 354 419 440 460 465 
# 
# $influence$cutoff
# [1] 0.007968127
```

The drop in test statistics or diagnostic plots indicates whether the
remedy helped.

## Statistical Accuracy and Validation

The `heteroTests` package implementations have been validated against:

- **Original published papers** with known test cases
- **Established R packages** (lmtest, car, stats)  
- **Commercial software** results (Stata, EViews)
- **Simulation studies** with known statistical properties

### Cross-Package Validation

``` r

# Compare our Breusch-Pagan test with lmtest
library(lmtest)
# Loading required package: zoo
# 
# Attaching package: 'zoo'
# The following objects are masked from 'package:base':
# 
#     as.Date, as.Date.numeric
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)

# Our implementation
our_result <- performBreuschPaganTest(model, mtcars)
# [INFO] Running Breusch-Pagan test

# Reference implementation
ref_result <- bptest(model)

# Results should match
print(paste("Our statistic:", round(our_result$statistic, 6)))
# [1] "Our statistic: 1.026766"
print(paste("Reference statistic:", round(ref_result$statistic, 6)))
# [1] "Reference statistic: 0.880722"
```

## Performance and Memory Considerations

### Large Datasets

The `heteroTests` package is designed to handle datasets of various
sizes, but some considerations apply:

- **Memory warnings**: Functions will warn when processing large
  datasets (\>50MB)
- **Computational time**: Some tests (White, Breusch-Pagan) can be slow
  on very large datasets
- **Memory usage**: Peak memory usage is typically 2-3x the dataset size

### Recommendations for Large Datasets

``` r

# Illustrative only: `your_data` stands for your own data frame, so this
# chunk is not evaluated when the vignette is built.

# 1. Subset for initial exploration
large_subset <- your_data[sample(nrow(your_data), 1000), ]
quick_result <- performWhiteTest(model, large_subset)

# 2. Use simpler tests first
fast_result <- performGQTest(model, your_data, order_by = "x1")

# 3. Monitor memory usage
gc()  # Garbage collection before analysis
result <- performWhiteTest(model, your_data)
gc()  # Clean up after
```

### Memory Management Tips

- Use [`gc()`](https://rdrr.io/r/base/gc.html) to free memory between
  analyses
- Consider processing data in chunks for very large datasets
- Close unused objects with [`rm()`](https://rdrr.io/r/base/rm.html) to
  free memory
