#' Default pipeline components
#'
#' @description
#' Building blocks used by [instance_hdr()] by default. They are exported so
#' they can be reused, wrapped, or used as templates for custom components.
#'
#' * `y_linear_gaussian()` generates \eqn{y = X\beta + \sigma\varepsilon} with
#'   \eqn{\varepsilon \sim N(0, I_n)}.
#' * `fit_cv_glmnet()` fits [glmnet::cv.glmnet()]. The returned object also
#'   contains the full-data path fit in `$glmnet.fit`.
#' * `post_glmnet_coefs()` extracts coefficients at the cross-validated
#'   `lambda` and puts them next to the true coefficients.
#'
#' @param x Design matrix.
#' @param beta True coefficient vector.
#' @param sigma Noise standard deviation.
#' @param y Outcome vector.
#' @param ... Further arguments passed to [glmnet::cv.glmnet()], for example
#'   `alpha`, `nfolds` or `standardize`.
#' @param fit A `cv.glmnet` object.
#' @param s Which `lambda` to use: `"lambda.1se"` (default), `"lambda.min"`,
#'   or a numeric value. A numeric value not on the fitted path is handled
#'   by linear interpolation, as in [glmnet::coef.glmnet()].
#' @return
#' * `y_linear_gaussian()`: numeric vector of length `nrow(x)`.
#' * `fit_cv_glmnet()`: a `cv.glmnet` object.
#' * `post_glmnet_coefs()`: `data.frame` with columns `term`, `estimate`,
#'   and `true_beta`, one row per coefficient including the intercept (whose
#'   `true_beta` is 0). Attributes `lambda` and `lambda_type` record the
#'   penalty used.
#' @name components
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(50 * 20), 50, 20)
#' beta <- c(2, -2, rep(0, 18))
#' y <- y_linear_gaussian(x, beta, sigma = 0.5)
#' fit <- fit_cv_glmnet(x, y, nfolds = 5)
#' head(post_glmnet_coefs(fit, x, y, beta, s = "lambda.min"))
NULL

#' @rdname components
#' @export
y_linear_gaussian <- function(x, beta, sigma = 1) {
  drop(x %*% beta) + sigma * stats::rnorm(nrow(x))
}

#' @rdname components
#' @export
fit_cv_glmnet <- function(x, y, ...) {
  glmnet::cv.glmnet(x, y, ...)
}

#' @rdname components
#' @export
post_glmnet_coefs <- function(fit, x, y, beta, s = "lambda.1se") {
  if (!inherits(fit, "cv.glmnet")) {
    stop("`fit` must be a cv.glmnet object, not ", class(fit)[1], ".",
         call. = FALSE)
  }
  lambda <- if (is.character(s)) fit[[s]] else s
  if (is.null(lambda)) stop("`s` = \"", s, "\" not found in `fit`.", call. = FALSE)
  beta_hat <- stats::coef(fit, s = s)
  terms <- rownames(beta_hat)
  output <- data.frame(
    term = terms,
    estimate = as.numeric(beta_hat),
    true_beta = if (terms[1] == "(Intercept)") c(0, beta) else beta
  )
  attr(output, "lambda") <- lambda
  attr(output, "lambda_type") <- if (is.character(s)) s else "user"
  output
}
