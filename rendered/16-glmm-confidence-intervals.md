
GLMM confidence intervals
=========================

Confidence intervals on random effects or on predictions including the random effects in GLMMs are complicated.

There is no one magic bullet if you are using the lme4 package. Various options make different assumptions and range from simple to complex.

The single best resource aggregating information on this topic (and other issues involving GLMMs in R) is <http://glmm.wikidot.com/faq>. It's well worth skimming.

Below we'll work through some the options.

``` r
confint(m, method = "profile")
confint(m, method = "Wald")
confint(m, method = "boot")
```

From <https://rpubs.com/bbolker/glmmchapter>

Population level predictions:

``` r
predict_pop_ci <- function(model, newdata, alpha = 0.05) {
  # baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model, re.form = NA, newdata = newdata)
  # fixed-effects model matrix for new data
  X <- model.matrix(formula(model, fixed.only = TRUE)[-2],
    newdata)
  beta <- fixef(model) # fixed-effects coefficients
  V <- vcov(model) # variance-covariance matrix of beta
  pred.se <-
    sqrt(diag(X %*% V %*% t(X))) # std errors of predictions
  if (identical(class(model), "glmerMod")) {
    linkinv <- model@resp$family$linkinv # inverse-link function
  } else {
    linkinv <- I
  }
  # construct 95% Normal CIs on the link scale and
  # transform back to the response scale:
  crit <- -qnorm(alpha / 2)
  linkinv(tibble::tibble(
    fit = pred0,
    lwr = pred0 - crit * pred.se,
    upr = pred0 + crit * pred.se
  ))
}
predict_pop_ci(m_slope, d)
confint(m_slope, method = "profile")
confint(m_slope, method = "Wald")
confint(m_slope, method = "boot")
```

``` r
library(simr)
(model1 <- lmer(y ~ x + (1|g), data=simdata))
fixef(model1)["x"] <- -0.1
ps1 <- powerSim(model1, nsim = 25)
print(ps1)

model2 <- extend(model1, along="x", n=20)
powerSim(model2, nsim = 25)

pc2 <- powerCurve(model2, nsim = 10)
plot(pc2)

model3 <- extend(model1, along="g", n=15) 
pc3 <- powerCurve(model3, along="g", nsim = 50) 
plot(pc3)

# The output for this analysis is shown in Fig. 3. To reach 80% power, we would need at least 11 sites.

model4 <- extend(model1, within="x+g", n=5)
pc4 <- powerCurve(model4, within="x+g", breaks=1:5, nsim = 50)
plot(pc4)
```
