
Distribution playground
=======================

Goals
=====

-   Review the most common probability distributions used in GLMs and GLMMs
-   Get a feeling for how the parameters affect the distribution shapes
-   Have an interactive resource to come back to as needed

Continuous distributions
========================

Normal
------

``` r
library(manipulate)

x <- seq(-10, 10, length.out = 300)
manipulate({
  y <- dnorm(
    x = x,
    mean = mean,
    sd = sd)
  plot(x = x, y = y, type = "l", ylim = c(0, max(y)),
    main = "Normal")},
  mean = slider(-10, 10, 0),
  sd = slider(0.1, 10, 2))
```

Log normal
----------

``` r
x <- seq(0, 100, length.out = 300)
manipulate({
  y <- dlnorm(
    x = x,
    meanlog = meanlog,
    sdlog = sdlog)
  plot(x = x, y = y, type = "l", xlim = c(0, 100), ylim = c(0, max(y)),
    main = "Lognormal")},
  meanlog = slider(0.1, 10, 3),
  sdlog = slider(0.01, 2, 1))
```

Gamma
-----

What happens if the Gamma shape parameter is large and the scale parameter is small?

What happens if the Gamma *scale* parameter is large and the *shape* parameter is small?

What distributions do these resemble? Why is that useful?

``` r
x <- seq(0, 100, length.out = 300)
manipulate({
  y <- dgamma(
    x = x,
    shape = shape,
    scale = scale)
  plot(x = x, y = y, type = "l", ylim = c(0, max(y)),
    main = "Gamma")},
  shape = slider(0.1, 10, 3),
  scale = slider(0.1, 60, 2))
```

Discrete distributions
======================

Poisson
-------

What happens to the width of the poisson as the mean (lambda) increases?

When might you expect to observe a poisson distribution?

``` r
x <- seq(0, 100)
manipulate({
  y <- dpois(x,
    lambda = lambda)
  plot(x = x, y = y, type = "h", xlim = c(0, 60), ylim = c(0, max(y)),
    main = "Poisson")},
  lambda = slider(0.1, 30, 3))
```

Negative binomial
-----------------

What happens when the `size` parameter gets really small? What distribution does this resemble if the `size` parameter gets big?

When might you expect to observe a negative binomial distribution?

``` r
x <- seq(0, 100)
manipulate({
  y <- dnbinom(
    x = x,
    size = size,
    mu = mu)
  plot(x = x, y = y, type = "h", ylim = c(0, max(y)),
    main = "Negative binomial")},
  size = slider(0.1, 10, 1),
  mu = slider(0.1, 60, 4))
```

Binomial
--------

Read the help `?dbinom` and play with the sliders. What do `size` and `prob` arguments mean?

Think in terms of flipping a coin. What would the `prob` parameter be for a typical coin? If I flipped a coin 6 times what would the parameter `size` be?

``` r
x <- seq(0, 10)
manipulate({
  y <- dbinom(x,
    size = size,
    prob = prob)
  plot(x = x, y = y, type = "h", xlim = c(0, 20), ylim = c(0, max(y)),
    main = "Binomial")},
  size = slider(1, 7, initial = 1, step = 1),
  prob = slider(0, 1, initial = 0.5, step = 0.1))
```
