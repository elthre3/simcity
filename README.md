# simcity

<!-- badges: start -->
[![R-CMD-check](https://github.com/joftius/simcity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joftius/simcity/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of simcity is to streamline simulating data to evaluate and compare methods for some common statistical tasks. 

## Installation

You can install the development version of simcity from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("joftius/simcity")
```

## Example

The first motivating example for this package was simulating high-dimensional linear regression models, fitting them with the lasso, and analyzing various performance metrics.

``` r
library(simcity)
n <- 100
p <- 200
s0 <- 5
one_lasso_fit <- instance_hdr(n, p, s0)
head(one_lasso_fit)
```

The above example generates one instance of simulated data with `hdi::rXb()`. It then fits a lasso with `glmnet::cv.glmnet()` and returns the true and estimated coefficients at `lambda.1se`.

`simulate_hdr()` repeats this many times, in parallel across `cores` worker processes. Each replication gets its own random number stream, so results depend on `seed` but not on the number of cores. `simmary_coefs()` computes support recovery and estimation metrics for each replication.

``` r
library(ggplot2)
niters <- 200
many_lasso_fits <- simulate_hdr(niters, n, p, s0, cores = 2, seed = 1)
sim_summary <- simmary_coefs(many_lasso_fits)
head(sim_summary)
ggplot(sim_summary, aes(screened, beta_min)) + geom_boxplot()
ggplot(sim_summary, aes(beta_min, mse)) + geom_point()
```

## Custom pipelines

Each step can be replaced. The outcome generator, the fitting method and the post-processing are ordinary functions, and extra arguments to each are passed as named lists:

``` r
# Elastic net, noisier outcomes, lambda.min, equicorrelated design
sims <- simulate_hdr(
  100, n, p, s0,
  xtype = "equi.corr", x.par = 0.5,
  yargs = list(sigma = 2),
  fitargs = list(alpha = 0.5),
  postargs = list(s = "lambda.min"),
  seed = 1
)

# Any method: here forward stepwise selection (requires the lars package)
fit_fs <- function(x, y) lars::lars(x, y, type = "stepwise", max.steps = 10)
post_fs <- function(fit, x, y, beta) {
  est <- coef(fit, s = 10, mode = "step")
  data.frame(estimate = est, true_beta = beta)
}
sims_fs <- simulate_hdr(100, n, p, s0, fitfun = fit_fs, postfun = post_fs,
                        seed = 1)
simmary_coefs(sims_fs)
```

See `vignette("lasso-screening")` and `vignette("parallel-computation")` for more.
