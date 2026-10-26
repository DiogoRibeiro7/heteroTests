# a suite prints as a table, and its summary as summary.lm() does

    Code
      s
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt + qsec, data = mtcars), n = 32
      
                    Statistic df p-value  
      white            11.822  5  0.0373 *
      koenker           3.086  2  0.2138  
      cook_weisberg     0.591  1  0.4420  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    Code
      summary(s)
    Output
      
      Call:
      lm(formula = mpg ~ wt + qsec, data = mtcars)
      
      Number of observations: 32
      
      Tests:
                    Statistic df p-value  
      white            11.822  5  0.0373 *
      koenker           3.086  2  0.2138  
      cook_weisberg     0.591  1  0.4420  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

# a fallback is named in the notes

    Code
      s
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt, data = small), n = 18
      
              Statistic df p-value
      white       1.942  1   0.163
      koenker     1.413  1   0.235
      
      Notes:
        white refused; "breusch_pagan" ran in its place: Only 18 observations det...
    Code
      summary(s)
    Output
      
      Call:
      lm(formula = mpg ~ wt, data = small)
      
      Number of observations: 18
      
      Tests:
              Statistic df p-value
      white       1.942  1   0.163
      koenker     1.413  1   0.235
      
      Notes:
        white refused, and "breusch_pagan" ran in its place (Breusch-Pagan test for
          heteroscedasticity). The refusal: Only 18 observations detected but White
          requires at least 20. Provide more data or choose a different diagnostic.
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

# a failed or refused test shows NA and its message

    Code
      s
    Output
      Heteroscedasticity tests of glm(formula = am ~ wt, family = binomial, data =
          mtcars), n = 32
      
              Statistic df p-value
      white          NA NA      NA
      koenker        NA NA      NA
      
      Notes:
        white refused: performWhiteTest() needs a model fitted by stats::lm(): a ...
        koenker refused: performKoenkerTest() needs a model fitted by stats::lm()...
    Code
      summary(s)
    Output
      
      Call:
      glm(formula = am ~ wt, family = binomial, data = mtcars)
      
      Number of observations: 32
      Family: binomial (logit link)
      
      Tests:
              Statistic df p-value
      white          NA NA      NA
      koenker        NA NA      NA
      
      Notes:
        white refused: performWhiteTest() needs a model fitted by stats::lm(): a
          glm with the binomial family and the logit link is not a least-squares
          fit. A glm with the gaussian family and the identity link is accepted.
        koenker refused: performKoenkerTest() needs a model fitted by stats::lm():
          a glm with the binomial family and the logit link is not a least-squares
          fit. A glm with the gaussian family and the identity link is accepted.
      

# the significance codes follow show.signif.stars, and digits is respected

    Code
      print(s, signif.stars = FALSE)
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt + qsec, data = mtcars), n = 32
      
              Statistic df p-value
      white      11.822  5  0.0373
      koenker     3.086  2  0.2138
    Code
      print(summary(s), digits = 6)
    Output
      
      Call:
      lm(formula = mpg ~ wt + qsec, data = mtcars)
      
      Number of observations: 32
      
      Tests:
              Statistic df  p-value  
      white    11.82248  5 0.037303 *
      koenker   3.08584  2 0.213756  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

---

    Code
      s
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt + qsec, data = mtcars), n = 32
      
              Statistic df p-value
      white      11.822  5  0.0373
      koenker     3.086  2  0.2138
    Code
      summary(s)
    Output
      
      Call:
      lm(formula = mpg ~ wt + qsec, data = mtcars)
      
      Number of observations: 32
      
      Tests:
              Statistic df p-value
      white      11.822  5  0.0373
      koenker     3.086  2  0.2138
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

# parameters that are not degrees of freedom go to the notes

    Code
      s
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt + qsec, data = mtcars), n = 32
      
                    Statistic df df2 p-value  
      white            11.822  5  NA  0.0373 *
      variance_form     1.765  3  26  0.1786  
      szroeter          1.137 NA  NA  0.1278  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Notes:
        szroeter's n = 32 is not a degree of freedom.

# an empty suite and an all-failed suite print

    Code
      empty
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt + qsec, data = mtcars), n = 32
      
      No tests.
    Code
      summary(empty)
    Output
      
      Call:
      lm(formula = mpg ~ wt + qsec, data = mtcars)
      
      Number of observations: 32
      
      Tests:
      No tests.
      
    Code
      summary(failed)
    Output
      
      Call:
      glm(formula = am ~ wt, family = binomial, data = mtcars)
      
      Number of observations: 32
      Family: binomial (logit link)
      
      Tests:
            Statistic df p-value
      white        NA NA      NA
      
      Notes:
        white refused: performWhiteTest() needs a model fitted by stats::lm(): a
          glm with the binomial family and the logit link is not a least-squares
          fit. A glm with the gaussian family and the identity link is accepted.
      

# a grouped suite prints one table, with the status of each group

    Code
      g
    Output
      Heteroscedasticity tests of mpg ~ wt, by cyl: 3 groups, n = 7 to 14
      
                     Statistic df p-value
      cyl=4: white       0.003  1   0.958
      cyl=4: koenker     0.003  1   0.958
      cyl=6: white          NA NA      NA
      cyl=6: koenker        NA NA      NA
      cyl=8: white       0.243  1   0.622
      cyl=8: koenker     0.243  1   0.622
      
      Notes:
        cyl=4: white refused; "ncv" ran in its place: Only 11 observations detect...
        cyl=4: koenker refused; "ncv" ran in its place: Only 11 observations dete...
        cyl=6: white refused: Only 7 observations detected but White requires at ...
        cyl=6: koenker refused: Only 7 observations detected but Koenker requires...
        cyl=8: white refused; "ncv" ran in its place: Only 14 observations detect...
        cyl=8: koenker refused; "ncv" ran in its place: Only 14 observations dete...
    Code
      summary(g)
    Output
      
      Formula:
      mpg ~ wt
      
      Groups: 3, by cyl
        cyl=4  n = 11
        cyl=6  n =  7
        cyl=8  n = 14
      
      Tests:
                     Statistic df p-value
      cyl=4: white       0.003  1   0.958
      cyl=4: koenker     0.003  1   0.958
      cyl=6: white          NA NA      NA
      cyl=6: koenker        NA NA      NA
      cyl=8: white       0.243  1   0.622
      cyl=8: koenker     0.243  1   0.622
      
      Notes:
        cyl=4: white refused, and "ncv" ran in its place (Cook-Weisberg score test
          for non-constant variance). The refusal: Only 11 observations detected
          but White requires at least 20. Provide more data or choose a different
          diagnostic.
        cyl=4: koenker refused, and "ncv" ran in its place (Cook-Weisberg score
          test for non-constant variance). The refusal: Only 11 observations
          detected but Koenker requires at least 15. Provide more data or choose a
          different diagnostic.
        cyl=6: white refused: Only 7 observations detected but White requires at
          least 20. Provide more data or choose a different diagnostic.
        cyl=6: koenker refused: Only 7 observations detected but Koenker requires
          at least 15. Provide more data or choose a different diagnostic.
        cyl=8: white refused, and "ncv" ran in its place (Cook-Weisberg score test
          for non-constant variance). The refusal: Only 14 observations detected
          but White requires at least 20. Provide more data or choose a different
          diagnostic.
        cyl=8: koenker refused, and "ncv" ran in its place (Cook-Weisberg score
          test for non-constant variance). The refusal: Only 14 observations
          detected but Koenker requires at least 15. Provide more data or choose a
          different diagnostic.
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

# arguments given to print() replace those passed to printCoefmat()

    Code
      print(failed, na.print = "-")
    Output
      Heteroscedasticity tests of glm(formula = am ~ wt, family = binomial, data =
          mtcars), n = 32
      
              Statistic df p-value
      white           -  -       -
      koenker         -  -       -
      
      Notes:
        white refused: performWhiteTest() needs a model fitted by stats::lm(): a ...
        koenker refused: performKoenkerTest() needs a model fitted by stats::lm()...
    Code
      print(s, P.values = FALSE)
    Output
      Heteroscedasticity tests of lm(formula = mpg ~ wt + qsec, data = mtcars), n = 32
      
              Statistic df
      white      11.822  5
      koenker     3.086  2

