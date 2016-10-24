
R formula and random effect grouping syntax
===========================================

Goals
=====

-   Review R formula syntax including some lesser known shortcuts
-   Be able to identify possible random effect grouping structures
-   Be able to write out random effect formulas for `lme4::lmer()`
-   Be able to convert `lme4::lmer()` random effect syntax to `nlme::lme()` syntax

Formula syntax
==============

A formula in R uses the `~` character. E.g.

``` r
f <- y ~ x
class(f)
```

    ## [1] "formula"

We covered R's formula syntax in the slides. Below are some exercises to solidify your understanding.

Challenge 1
-----------

For this course, I'm assuming you're already somewhat familiar with R formulas. Let's review some more edge-case R formula syntax quickly. Read `?formula` if needed.

Re-write the following in shortform:

``` r
y ~ x1 + x2 + x1:x2
```

    ## y ~ x1 + x2 + x1:x2
    ## <environment: 0x7fe03ab99fc8>

Answer:

``` r
y ~ x1 * x2 # exercise
```

    ## y ~ x1 * x2
    ## <environment: 0x7fe03ab99fc8>

What is the longhand way of writing the following?

``` r
y ~ (x1 + x2 + x3)^2
```

    ## y ~ (x1 + x2 + x3)^2
    ## <environment: 0x7fe03ab99fc8>

Answer:

``` r
y ~ x1 + x2 + x3 + x1:x2 + x2:x3 + x1:x3 # exercise
```

    ## y ~ x1 + x2 + x3 + x1:x2 + x2:x3 + x1:x3
    ## <environment: 0x7fe03ab99fc8>

Are the following the same? Why or why not?

``` r
y ~ x^2
```

    ## y ~ x^2
    ## <environment: 0x7fe03ab99fc8>

``` r
y ~ I(x^2)
```

    ## y ~ I(x^2)
    ## <environment: 0x7fe03ab99fc8>

What does `y ~ x^2` actually mean in simplified form?

``` r
y ~ x # exercise
```

    ## y ~ x
    ## <environment: 0x7fe03ab99fc8>

What are 2 ways to remove the intercept in an R formula? Hint: read `?formula`. When might you want to do this?

``` r
y ~ -1 + x # exercise
```

    ## y ~ -1 + x
    ## <environment: 0x7fe03ab99fc8>

``` r
y ~ 0 + x # exercise
```

    ## y ~ 0 + x
    ## <environment: 0x7fe03ab99fc8>

Random effect syntax
====================

We'll cover random effect syntax in the slides. Below are some exercises.

Challenge 2
-----------

A group of grad students are trying to model log(frog density) as a function of vegetation characteristics. What might their model look like in the following cases (using `lme4::lmer`)? Give cases with random intercepts only and cases with random intercepts and slopes.

Jerry measures frog density and vegetation characteristics from 1 transect (`transect`) within 6 separate ponds (`pond`).

``` r
# random intercepts
log(frog_dens) ~ vegetation + 
  (1 | pond) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 | pond)
    ## <environment: 0x7fe03ab99fc8>

``` r
# random slopes and intercepts:
log(frog_dens) ~ vegetation + 
  (1 + vegetation | pond) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 + vegetation | pond)
    ## <environment: 0x7fe03ab99fc8>

Emily measures frog density and vegetation characteristics from 5 transects within 1 pond.

``` r
log(frog_dens) ~ vegetation + 
  (1 | transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 | transect)
    ## <environment: 0x7fe03ab99fc8>

``` r
log(frog_dens) ~ vegetation + 
  (1 + vegetation | transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 + vegetation | transect)
    ## <environment: 0x7fe03ab99fc8>

Bonus: what are 2 ways you can write the above models that have a random slope? Hint: do you need to specify the random intercept explicitely? Are there advantages/disadvantages to each syntax?

``` r
log(frog_dens) ~ vegetation + (1 + vegetation | transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 + vegetation | transect)
    ## <environment: 0x7fe03ab99fc8>

``` r
log(frog_dens) ~ vegetation + (vegetation | transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (vegetation | transect)
    ## <environment: 0x7fe03ab99fc8>

Bonus 2: How can you specify one of the above models with a random slope but no random intercept? Hint, see the examples in `?lmer`. When might you use this?

``` r
log(frog_dens) ~ vegetation + (0 + vegetation | transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (0 + vegetation | transect)
    ## <environment: 0x7fe03ab99fc8>

Challenge 3
-----------

Jane measures frog density and vegetation characteristics from 2 transects (`transect`) within 4 ponds (`pond`). Assume her data are structured like this:

``` r
set.seed(99)
d <- tibble::tibble(
  frog_dens = rlnorm(100), 
  vegetation = rnorm(100), 
  pond = gl(25, 4), # generates factor levels
  transect = rep(gl(2, 2), 25))
d
```

    ## # A tibble: 100 × 4
    ##    frog_dens vegetation   pond transect
    ##        <dbl>      <dbl> <fctr>   <fctr>
    ## 1  1.2385762 -1.5250694      1        1
    ## 2  1.6155220 -0.5009917      1        1
    ## 3  1.0918011 -1.2131812      1        2
    ## 4  1.5587099 -0.6302768      1        2
    ## 5  0.6956992 -1.4474855      2        1
    ## 6  1.1305158 -0.1669084      2        1
    ## 7  0.4215381  1.5889265      2        2
    ## 8  1.6317030 -0.2303574      2        2
    ## 9  0.6948100 -0.5733715      3        1
    ## 10 0.2741056  0.5627325      3        1
    ## # ... with 90 more rows

*Ignoring* that Jane probably hasn't sampled enough transects to get much benefit from a mixed effect model (we're trying to keep this example data set small):

What is one way she could write the model with a random intercept for transects nested within ponds?

``` r
log(frog_dens) ~ vegetation + 
  (1 | pond/transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 | pond/transect)
    ## <environment: 0x7fe03ab99fc8>

What's another way to write the same model?

``` r
log(frog_dens) ~ vegetation + 
  (1 | pond) + (1 | pond:transect) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 | pond) + (1 | pond:transect)
    ## <environment: 0x7fe03ab99fc8>

What if she had coded her transects as in the `transect2` column in the following code chunk?

``` r
d <- d %>% mutate(transect2 = gl(50, 2))
```

She could write her model the same way as above with `transect`, but she has one new option. What is that?

``` r
log(frog_dens) ~ vegetation + 
  (1 | pond) + (1 | transect2) # exercise
```

    ## log(frog_dens) ~ vegetation + (1 | pond) + (1 | transect2)
    ## <environment: 0x7fe03ab99fc8>

Let's paste those into `lmer()` for proof that they are all the same. First, fill in the last line with the answer to the last exercise. Note that the surrounding parentheses just cause R to print the output while running the line.

``` r
library(lme4)
(m1 <- lmer(log(frog_dens) ~ vegetation + (1 | pond/transect), data = d))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(frog_dens) ~ vegetation + (1 | pond/transect)
    ##    Data: d
    ## REML criterion at convergence: 265.1362
    ## Random effects:
    ##  Groups        Name        Std.Dev.
    ##  transect:pond (Intercept) 0.2107  
    ##  pond          (Intercept) 0.3100  
    ##  Residual                  0.8243  
    ## Number of obs: 100, groups:  transect:pond, 50; pond, 25
    ## Fixed Effects:
    ## (Intercept)   vegetation  
    ##    -0.09739      0.07563

``` r
(m2 <- lmer(log(frog_dens) ~ vegetation + (1 | pond) + (1 | transect:pond), data = d))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(frog_dens) ~ vegetation + (1 | pond) + (1 | transect:pond)
    ##    Data: d
    ## REML criterion at convergence: 265.1362
    ## Random effects:
    ##  Groups        Name        Std.Dev.
    ##  transect:pond (Intercept) 0.2107  
    ##  pond          (Intercept) 0.3100  
    ##  Residual                  0.8243  
    ## Number of obs: 100, groups:  transect:pond, 50; pond, 25
    ## Fixed Effects:
    ## (Intercept)   vegetation  
    ##    -0.09739      0.07563

``` r
(m3 <- lmer(log(frog_dens) ~ vegetation +  
    (1 | pond) + (1 | transect2), data = d)) # exercise 
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(frog_dens) ~ vegetation + (1 | pond) + (1 | transect2)
    ##    Data: d
    ## REML criterion at convergence: 265.1362
    ## Random effects:
    ##  Groups    Name        Std.Dev.
    ##  transect2 (Intercept) 0.2107  
    ##  pond      (Intercept) 0.3100  
    ##  Residual              0.8243  
    ## Number of obs: 100, groups:  transect2, 50; pond, 25
    ## Fixed Effects:
    ## (Intercept)   vegetation  
    ##    -0.09739      0.07563

Bonus: but the following *is not* the same, and is probably *not* what you want to do. Why is that? What does this one mean?

``` r
(m_wrong <- lmer(log(frog_dens) ~ vegetation + (1 | pond) + (1 | transect), data = d))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(frog_dens) ~ vegetation + (1 | pond) + (1 | transect)
    ##    Data: d
    ## REML criterion at convergence: 265.2606
    ## Random effects:
    ##  Groups   Name        Std.Dev.
    ##  pond     (Intercept) 0.33431 
    ##  transect (Intercept) 0.02559 
    ##  Residual             0.84145 
    ## Number of obs: 100, groups:  pond, 25; transect, 2
    ## Fixed Effects:
    ## (Intercept)   vegetation  
    ##    -0.09701      0.07995

Bonus 2: can you show what `m_wrong` is estimating as random effects for `transect`? Compare that to `m3`. Why is this happening?

``` r
ranef(m_wrong) # exercise
```

    ## $pond
    ##     (Intercept)
    ## 1   0.186036274
    ## 2  -0.019921440
    ## 3  -0.119693791
    ## 4  -0.399903107
    ## 5  -0.061903199
    ## 6   0.190231802
    ## 7   0.108045124
    ## 8   0.213646811
    ## 9  -0.329184379
    ## 10  0.067913315
    ## 11 -0.314129699
    ## 12 -0.391101615
    ## 13  0.101501893
    ## 14 -0.099693424
    ## 15 -0.141960780
    ## 16 -0.007980792
    ## 17 -0.162358560
    ## 18  0.134328764
    ## 19 -0.085944032
    ## 20  0.189277332
    ## 21  0.078897812
    ## 22  0.338183400
    ## 23  0.089786962
    ## 24  0.269194475
    ## 25  0.166730855
    ## 
    ## $transect
    ##    (Intercept)
    ## 1  0.003783107
    ## 2 -0.003783107

``` r
ranef(m3) # exercise
```

    ## $transect2
    ##      (Intercept)
    ## 1   4.182231e-02
    ## 2   3.166406e-02
    ## 3   6.414268e-03
    ## 4  -1.433038e-02
    ## 5  -7.271655e-02
    ## 6   2.541446e-02
    ## 7  -3.621300e-02
    ## 8  -1.235329e-01
    ## 9  -9.714956e-02
    ## 10  7.184924e-02
    ## 11  9.512193e-02
    ## 12 -1.959282e-02
    ## 13  4.155279e-02
    ## 14  1.907079e-03
    ## 15 -9.294960e-03
    ## 16  9.449362e-02
    ## 17  3.528317e-02
    ## 18 -1.660621e-01
    ## 19 -8.922335e-03
    ## 20  3.595863e-02
    ## 21 -5.640365e-02
    ## 22 -6.807567e-02
    ## 23 -1.025712e-01
    ## 24 -5.328818e-02
    ## 25  8.429647e-02
    ## 26 -4.405713e-02
    ## 27  5.826381e-03
    ## 28 -4.506176e-02
    ## 29  1.493394e-02
    ## 30 -7.079095e-02
    ## 31 -4.690704e-06
    ## 32 -3.102700e-03
    ## 33 -1.017041e-01
    ## 34  3.684620e-02
    ## 35 -3.239774e-03
    ## 36  5.684185e-02
    ## 37 -1.460073e-02
    ## 38 -1.937337e-02
    ## 39  1.117644e-01
    ## 40 -3.600555e-02
    ## 41  3.414888e-02
    ## 42 -3.334486e-03
    ## 43  7.863863e-02
    ## 44  5.582663e-02
    ## 45  3.690783e-02
    ## 46 -6.720198e-04
    ## 47  1.709668e-01
    ## 48 -6.452289e-02
    ## 49 -8.704128e-03
    ## 50  7.484817e-02
    ## 
    ## $pond
    ##     (Intercept)
    ## 1   0.159037986
    ## 2  -0.017131914
    ## 3  -0.102370420
    ## 4  -0.345719367
    ## 5  -0.054754544
    ## 6   0.163458857
    ## 7   0.094055147
    ## 8   0.184385539
    ## 9  -0.283029726
    ## 10  0.058511509
    ## 11 -0.269396108
    ## 12 -0.337308347
    ## 13  0.087085327
    ## 14 -0.084912560
    ## 15 -0.120884834
    ## 16 -0.006724964
    ## 17 -0.140364456
    ## 18  0.116004727
    ## 19 -0.073526209
    ## 20  0.163955957
    ## 21  0.066688004
    ## 22  0.291007515
    ## 23  0.078420942
    ## 24  0.230364091
    ## 25  0.143147849

Challenge 4
-----------

For practice, how can we write the following model using `nlme::lme()` syntax?

lme4:

``` r
(m_lmer <- lmer(log(frog_dens) ~ vegetation + (1 | pond/transect), data = d))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log(frog_dens) ~ vegetation + (1 | pond/transect)
    ##    Data: d
    ## REML criterion at convergence: 265.1362
    ## Random effects:
    ##  Groups        Name        Std.Dev.
    ##  transect:pond (Intercept) 0.2107  
    ##  pond          (Intercept) 0.3100  
    ##  Residual                  0.8243  
    ## Number of obs: 100, groups:  transect:pond, 50; pond, 25
    ## Fixed Effects:
    ## (Intercept)   vegetation  
    ##    -0.09739      0.07563

nlme:

``` r
(m_nlme <- nlme::lme(log(frog_dens) ~ vegetation, 
  random =  ~ 1 | pond/transect, data = d)) # exercise
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##   Log-restricted-likelihood: -132.5681
    ##   Fixed: log(frog_dens) ~ vegetation 
    ## (Intercept)  vegetation 
    ## -0.09738925  0.07562702 
    ## 
    ## Random effects:
    ##  Formula: ~1 | pond
    ##         (Intercept)
    ## StdDev:   0.3099965
    ## 
    ##  Formula: ~1 | transect %in% pond
    ##         (Intercept)  Residual
    ## StdDev:    0.210722 0.8242575
    ## 
    ## Number of Observations: 100
    ## Number of Groups: 
    ##               pond transect %in% pond 
    ##                 25                 50

Compare the output. Are they the same?
