#' Generate a design matrix and sparse coefficient vector
#'
#' Internal replacement for `hdi::rXb()`, which is no longer on CRAN. It
#' follows the same generative model and makes the same random draws in the
#' same order, so a given seed gives the same `x` and `beta` as `hdi::rXb()`
#' did (up to platform-dependent floating point in the eigendecomposition
#' used by [MASS::mvrnorm()]).
#'
#' Rows of `x` are i.i.d. \eqn{N_p(0, \Sigma)}, with
#' * `"toeplitz"`: \eqn{\Sigma_{jk} = \rho^{|j-k|}}, `x.par = rho`;
#' * `"equi.corr"`: \eqn{\Sigma_{jk} = \rho} for \eqn{j \neq k}, unit
#'   diagonal, `x.par = rho`;
#' * `"exp.decay"`: \eqn{\Sigma = K^{-1}} with
#'   \eqn{K_{jk} = a^{|j-k|/b}}, `x.par = c(a, b)`. Note it is the
#'   *precision* matrix that decays exponentially.
#'
#' `beta` has its first `s0` coordinates nonzero, drawn i.i.d. uniform on
#' `[a, b]` for `btype = "U[a,b]"` or all equal to `c` for `btype = "bfixc"`.
#'
#' @param n,p,s0 Sample size, number of predictors, number of nonzero
#'   coefficients.
#' @param xtype One of `"toeplitz"`, `"exp.decay"`, `"equi.corr"`.
#' @param btype Coefficient distribution, `"U[a,b]"` or `"bfixc"`.
#' @param permuted If `TRUE`, randomly permute the columns of `x` so the
#'   active variables are not adjacent in the correlation structure.
#' @param x.par Covariance parameter(s), see above.
#' @return A list with elements `x` (an `n` by `p` matrix) and `beta`.
#' @noRd
r_design <- function(n, p, s0, xtype, btype, permuted, x.par) {
  x <- r_design_x(n, p, xtype, permuted, x.par)
  beta <- r_design_beta(p, s0, btype)
  list(x = x, beta = beta)
}

r_design_x <- function(n, p, xtype, permuted, x.par) {
  if (!is.numeric(x.par) || any(!is.finite(x.par))) {
    stop("`x.par` must be finite and numeric.", call. = FALSE)
  }
  n_par <- if (xtype == "exp.decay") 2 else 1
  if (length(x.par) != n_par) {
    stop("`x.par` must have length ", n_par, " when `xtype = \"", xtype,
         "\"`.", call. = FALSE)
  }
  lags <- abs(stats::toeplitz(0:(p - 1)))
  sigma <- switch(xtype,
    "toeplitz" = x.par^lags,
    "equi.corr" = {
      s <- matrix(x.par, p, p)
      diag(s) <- 1
      s
    },
    "exp.decay" = solve(x.par[1]^(lags / x.par[2]))
  )
  # hdi::rXb() inverted the Toeplitz and equicorrelation matrices twice. The
  # result differs from `sigma` only by rounding error, but those differences
  # change the eigenvectors used by MASS::mvrnorm(). Keeping the double
  # inversion keeps simulated data identical to earlier versions of simcity.
  if (xtype != "exp.decay") sigma <- solve(solve(sigma))

  x <- MASS::mvrnorm(n, rep(0, p), sigma)
  # mvrnorm() drops to a vector when n = 1
  if (n == 1) x <- matrix(x, nrow = 1, ncol = p)
  if (permuted) x <- x[, sample.int(p), drop = FALSE]
  x
}

r_design_beta <- function(p, s0, btype) {
  bad_btype <- paste0(
    "`btype` must be \"U[a,b]\" for uniform coefficients on [a, b], ",
    "or \"bfixc\" for coefficients all equal to c."
  )
  if (!is.character(btype) || length(btype) != 1) {
    stop(bad_btype, call. = FALSE)
  }
  if (grepl("^U\\[[^,]*,[^,]*\\]$", btype)) {
    bounds <- suppressWarnings(as.numeric(
      strsplit(gsub("^U\\[|\\]$", "", btype), ",")[[1]]
    ))
    if (length(bounds) != 2 || anyNA(bounds)) stop(bad_btype, call. = FALSE)
    b <- stats::runif(s0, bounds[1], bounds[2])
  } else if (grepl("^bfix", btype)) {
    value <- suppressWarnings(as.numeric(sub("^bfix", "", btype)))
    if (is.na(value)) stop(bad_btype, call. = FALSE)
    b <- rep(value, s0)
  } else {
    stop(bad_btype, call. = FALSE)
  }
  c(b, rep(0, p - s0))
}
