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
        white: failed; "breusch_pagan" ran in its place: Only 18 observations det...
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
        white: failed, and "breusch_pagan" ran in its place (Breusch-Pagan test for
          heteroscedasticity). It failed with: Only 18 observations detected but
          White requires at least 20. Provide more data or choose a different
          diagnostic.
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

# a failed or refused test shows NA and its message

    Code
      s
    Output
      Heteroscedasticity tests of glm(formula = am ~ wt, family = binomial, data = mtcars), n = 32
      
              Statistic df p-value
      white          NA NA      NA
      koenker        NA NA      NA
      
      Notes:
        white: failed: performWhiteTest() needs a model fitted by stats::lm(): a ...
        koenker: failed: performKoenkerTest() needs a model fitted by stats::lm()...
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
        white: failed (diagnostic_failure): performWhiteTest() needs a model fitted
          by stats::lm(): a glm with the binomial family and the logit link is not
          a least-squares fit. A glm with the gaussian family and the identity link
          is accepted.
        koenker: failed (diagnostic_failure): performKoenkerTest() needs a model
          fitted by stats::lm(): a glm with the binomial family and the logit link
          is not a least-squares fit. A glm with the gaussian family and the
          identity link is accepted.
      

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
      szroeter          1.106 NA  NA  0.1344  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Notes:
        Parameters that are not degrees of freedom: szroeter n = 32.

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
        white: failed (diagnostic_failure): performWhiteTest() needs a model fitted
          by stats::lm(): a glm with the binomial family and the logit link is not
          a least-squares fit. A glm with the gaussian family and the identity link
          is accepted.
      

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
        cyl=4 white: failed; "ncv" ran in its place: Only 11 observations detecte...
        cyl=4 koenker: failed; "ncv" ran in its place: Only 11 observations detec...
        cyl=6 white: failed: Only 7 observations detected but White requires at l...
        cyl=6 koenker: failed: Only 7 observations detected but Koenker requires ...
        cyl=8 white: failed; "ncv" ran in its place: Only 14 observations detecte...
        cyl=8 koenker: failed; "ncv" ran in its place: Only 14 observations detec...
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
        cyl=4 white: failed, and "ncv" ran in its place (Cook-Weisberg score test
          for non-constant variance). It failed with: Only 11 observations detected
          but White requires at least 20. Provide more data or choose a different
          diagnostic.
        cyl=4 koenker: failed, and "ncv" ran in its place (Cook-Weisberg score test
          for non-constant variance). It failed with: Only 11 observations detected
          but Koenker requires at least 15. Provide more data or choose a different
          diagnostic.
        cyl=6 white: failed (diagnostic_failure): Only 7 observations detected but
          White requires at least 20. Provide more data or choose a different
          diagnostic.
        cyl=6 koenker: failed (diagnostic_failure): Only 7 observations detected
          but Koenker requires at least 15. Provide more data or choose a different
          diagnostic.
        cyl=8 white: failed, and "ncv" ran in its place (Cook-Weisberg score test
          for non-constant variance). It failed with: Only 14 observations detected
          but White requires at least 20. Provide more data or choose a different
          diagnostic.
        cyl=8 koenker: failed, and "ncv" ran in its place (Cook-Weisberg score test
          for non-constant variance). It failed with: Only 14 observations detected
          but Koenker requires at least 15. Provide more data or choose a different
          diagnostic.
      
      Several tests of one model are not independent evidence: they are computed
      from the same residuals.
      

