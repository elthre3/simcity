test_that("r_design returns x and beta with the requested shape and support", {
  set.seed(1)
  d <- r_design(20, 8, 3, "toeplitz", "U[-2,2]", permuted = TRUE, x.par = 0.5)
  expect_equal(dim(d$x), c(20, 8))
  expect_length(d$beta, 8)
  expect_true(all(d$beta[1:3] != 0))
  expect_true(all(d$beta[4:8] == 0))
  expect_true(all(abs(d$beta) <= 2))
})

test_that("r_design is reproducible given the seed", {
  set.seed(3)
  a <- r_design(10, 5, 2, "equi.corr", "U[0,1]", TRUE, 0.3)
  set.seed(3)
  b <- r_design(10, 5, 2, "equi.corr", "U[0,1]", TRUE, 0.3)
  expect_identical(a, b)
})

test_that("design covariances have the documented structure", {
  set.seed(1)
  n <- 20000
  x <- r_design_x(n, 4, "toeplitz", FALSE, 0.6)
  expect_equal(cov(x), 0.6^abs(toeplitz(0:3)), tolerance = 0.05)

  x <- r_design_x(n, 4, "equi.corr", FALSE, 0.4)
  target <- matrix(0.4, 4, 4)
  diag(target) <- 1
  expect_equal(cov(x), target, tolerance = 0.05)

  # For exp.decay it is the precision matrix that decays
  x <- r_design_x(n, 4, "exp.decay", FALSE, c(0.4, 5))
  expect_equal(solve(cov(x)), 0.4^(abs(toeplitz(0:3)) / 5), tolerance = 0.05)
})

test_that("permuted = TRUE permutes columns only", {
  set.seed(5)
  unpermuted <- r_design_x(6, 5, "toeplitz", FALSE, 0.5)
  set.seed(5)
  permuted <- r_design_x(6, 5, "toeplitz", TRUE, 0.5)
  set.seed(5)
  invisible(MASS::mvrnorm(6, rep(0, 5), diag(5)))
  perm <- sample.int(5)
  expect_equal(permuted, unpermuted[, perm])
})

test_that("btype parses uniform and fixed coefficients", {
  expect_equal(r_design_beta(5, 2, "bfix1"), c(1, 1, 0, 0, 0))
  expect_equal(r_design_beta(4, 3, "bfix-0.5"), c(-0.5, -0.5, -0.5, 0))
  set.seed(1)
  b <- r_design_beta(100, 100, "U[3,4]")
  expect_true(all(b >= 3 & b <= 4))
  expect_equal(r_design_beta(3, 0, "U[-2,2]"), c(0, 0, 0))
  expect_error(r_design_beta(5, 2, "N(0,1)"), "btype")
  expect_error(r_design_beta(5, 2, "U[a,2]"), "btype")
  expect_error(r_design_beta(5, 2, "bfixz"), "btype")
})

test_that("edge cases: n = 1 and bad x.par", {
  x <- r_design_x(1, 3, "toeplitz", TRUE, 0.5)
  expect_equal(dim(x), c(1, 3))
  expect_error(r_design_x(5, 3, "exp.decay", FALSE, 0.5), "length 2")
  expect_error(r_design_x(5, 3, "toeplitz", FALSE, NA_real_), "finite")
})
