test_that("instance_hdr returns coefficients with the true support", {
  set.seed(1)
  out <- instance_hdr(50, 100, 5)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("term", "estimate", "true_beta"))
  expect_equal(nrow(out), 101)
  expect_equal(out$term[1], "(Intercept)")
  expect_equal(which(out$true_beta[-1] != 0), 1:5)
  expect_equal(attr(out, "lambda_type"), "lambda.1se")
})

test_that("fitargs are forwarded as named arguments, not as weights", {
  set.seed(1)
  ridge <- instance_hdr(50, 30, 3, fitargs = list(alpha = 0))
  expect_true(all(ridge$estimate[-1] != 0))

  record_nfolds <- function(fit, x, y, beta) length(unique(fit$foldid))
  n_folds <- instance_hdr(50, 30, 3,
                          fitargs = list(nfolds = 4, keep = TRUE),
                          postfun = record_nfolds)
  expect_equal(n_folds, 4)
})

test_that("yargs and postargs are forwarded", {
  get_resid_sd <- function(fit, x, y, beta) sd(y - x %*% beta)
  set.seed(1)
  resid_sd <- instance_hdr(2000, 10, 2, yargs = list(sigma = 3),
                           fitfun = function(x, y) NULL,
                           postfun = get_resid_sd)
  expect_equal(resid_sd, 3, tolerance = 0.1)

  set.seed(1)
  out <- instance_hdr(50, 30, 3, postargs = list(s = "lambda.min"))
  expect_equal(attr(out, "lambda_type"), "lambda.min")
})

test_that("design parameters xtype, x.par and permuted are respected", {
  lag1_cor <- function(fit, x, y, beta) cor(x[, 1], x[, 2])
  null_fit <- function(x, y) NULL
  set.seed(1)
  unpermuted <- instance_hdr(5000, 5, 1, x.par = 0.8, permuted = FALSE,
                             fitfun = null_fit, postfun = lag1_cor)
  expect_equal(unpermuted, 0.8, tolerance = 0.05)

  equi <- instance_hdr(5000, 5, 1, xtype = "equi.corr", x.par = 0.5,
                       permuted = TRUE, fitfun = null_fit, postfun = lag1_cor)
  expect_equal(equi, 0.5, tolerance = 0.05)
})

test_that("custom components receive the documented arguments", {
  seen <- instance_hdr(
    20, 10, 2,
    yfun = function(x, beta, shift) drop(x %*% beta) + shift,
    yargs = list(shift = 100),
    fitfun = function(x, y) list(ybar = mean(y)),
    postfun = function(fit, x, y, beta) list(fit = fit, p = ncol(x), s0 = sum(beta != 0))
  )
  expect_gt(seen$fit$ybar, 90)
  expect_equal(seen$p, 10)
  expect_equal(seen$s0, 2)
})

test_that("invalid inputs give informative errors", {
  expect_error(instance_hdr(50, 10, 11), "at most `p`")
  expect_error(instance_hdr(0, 10, 1), "`n`")
  expect_error(instance_hdr(50, 10, 1, fitargs = list(0)), "must be named")
  expect_error(instance_hdr(50, 10, 1, yfun = "y"), "must be a function")
  expect_error(instance_hdr(50, 10, 1, xtype = "nope"))
})
