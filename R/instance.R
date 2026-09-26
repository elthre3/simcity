#' Simulate a single instance of a high-dimensional linear regression
#'
#' @description
#' Simulates data from a sparse high-dimensional linear model, fits a model to
#' it, and post-processes the fit. Each step is a replaceable function, so the
#' same pipeline can compare different outcome models, fitting methods, and
#' performance summaries.
#'
#' @details
#' The pipeline is:
#'
#' 1. Generate a design matrix `x` with i.i.d. \eqn{N_p(0, \Sigma)} rows
#'    and a coefficient vector `beta`, following the reference designs of
#'    Dezeure et al. (2015), formerly generated with `hdi::rXb()`. The
#'    support of `beta` is always the first `s0` coordinates;
#'    `permuted = TRUE` randomly permutes the *columns of `x`*,
#'    so that the active variables are not adjacent in the correlation
#'    structure.
#' 2. `y <- yfun(x, beta, <yargs>)`
#' 3. `fit <- fitfun(x, y, <fitargs>)`
#' 4. `postfun(fit, x, y, beta, <postargs>)` is returned.
#'
#' `yargs`, `fitargs` and `postargs` are named lists spliced into the
#' respective calls as extra arguments via [do.call()]. For example,
#' `fitargs = list(alpha = 0.5)` calls `fitfun(x, y, alpha = 0.5)`.
#'
#' The fitting function deliberately does not receive `beta`. Oracle
#' comparisons belong in `postfun`, which does.
#'
#' @param n Sample size, the number of rows of the design matrix.
#' @param p Number of predictors, the number of columns of the design matrix.
#' @param s0 Sparsity, the number of nonzero coefficients in the true model.
#' @param xtype Design covariance \eqn{\Sigma}. `"toeplitz"`:
#'   \eqn{\Sigma_{jk} = \rho^{|j-k|}}. `"equi.corr"`: \eqn{\Sigma_{jk} = \rho}
#'   for \eqn{j \neq k} with unit diagonal. `"exp.decay"`: \eqn{\Sigma = K^{-1}}
#'   with \eqn{K_{jk} = a^{|j-k|/b}}.
#' @param btype Distribution of the nonzero coefficients: `"U[a,b]"` for
#'   i.i.d. uniform on \eqn{[a, b]}, or `"bfixc"` for all equal to `c`
#'   (e.g. `"bfix1"`).
#' @param permuted If `TRUE`, randomly permute the columns of `x`.
#' @param x.par Parameter of the design covariance: \eqn{\rho} for
#'   `"toeplitz"` and `"equi.corr"`, `c(a, b)` for `"exp.decay"`.
#'   `NULL` (default) uses `1/3` for `"toeplitz"`, `1/20` for `"equi.corr"`,
#'   and `c(0.4, 5)` for `"exp.decay"`.
#' @param yfun Function `(x, beta, ...)` returning a numeric outcome vector of
#'   length `n`. Default [y_linear_gaussian()].
#' @param fitfun Function `(x, y, ...)` returning a fitted model.
#'   Default [fit_cv_glmnet()].
#' @param postfun Function `(fit, x, y, beta, ...)` summarizing the fit.
#'   Default [post_glmnet_coefs()].
#' @param yargs,fitargs,postargs Named lists of extra arguments for `yfun`,
#'   `fitfun` and `postfun`.
#' @return Whatever `postfun` returns. With the defaults, a `data.frame` with
#'   columns `term`, `estimate` and `true_beta` (see [post_glmnet_coefs()]).
#' @references Dezeure, R., Bühlmann, P., Meier, L. and Meinshausen, N.
#'   (2015). High-dimensional inference: confidence intervals, p-values and
#'   R-software hdi. *Statistical Science*, 30(4), 533--558.
#' @seealso [simulate_hdr()] to repeat this many times in parallel, and
#'   [simmary_coefs()] to summarize the results.
#' @export
#' @examples
#' set.seed(1)
#' fit <- instance_hdr(n = 50, p = 100, s0 = 5)
#' head(fit)
#'
#' # Ridge instead of lasso, noisier outcome, lambda.min instead of lambda.1se
#' fit <- instance_hdr(50, 100, 5,
#'                     yargs = list(sigma = 2),
#'                     fitargs = list(alpha = 0),
#'                     postargs = list(s = "lambda.min"))
instance_hdr <- function(
    n,
    p,
    s0,
    xtype = c("toeplitz", "exp.decay", "equi.corr"),
    btype = "U[-2,2]",
    permuted = TRUE,
    x.par = NULL,
    yfun = y_linear_gaussian,
    fitfun = fit_cv_glmnet,
    postfun = post_glmnet_coefs,
    yargs = list(),
    fitargs = list(),
    postargs = list())
{
  xtype <- match.arg(xtype)
  check_dims(n, p, s0)
  check_component(yfun, yargs, "yfun", "yargs")
  check_component(fitfun, fitargs, "fitfun", "fitargs")
  check_component(postfun, postargs, "postfun", "postargs")
  if (is.null(x.par)) x.par <- default_x_par(xtype)

  sim_data <- r_design(n = n, p = p, s0 = s0, xtype = xtype, btype = btype,
                       permuted = permuted, x.par = x.par)
  x <- sim_data$x
  beta <- sim_data$beta

  y <- do.call(yfun, c(list(x, beta), yargs))
  fit <- do.call(fitfun, c(list(x, y), fitargs))
  do.call(postfun, c(list(fit, x, y, beta), postargs))
}

default_x_par <- function(xtype) {
  switch(xtype,
         "toeplitz"  = 1/3,
         "equi.corr" = 1/20,
         "exp.decay" = c(0.4, 5))
}
