test_that("simulate_hdr is reproducible given a seed", {
  result1 <- simulate_hdr(4, 50, 100, 5, cores = 1, seed = 42)
  expect_length(result1, 4)

  set.seed(1)
  result2 <- simulate_hdr(4, 50, 100, 5, cores = 1, seed = 42)
  expect_identical(result1, result2)

  result3 <- simulate_hdr(4, 50, 100, 5, cores = 1, seed = 1)
  expect_false(identical(result1, result3))
})

test_that("seed = NULL follows set.seed()", {
  set.seed(7)
  a <- simulate_hdr(2, 30, 20, 2, cores = 1)
  set.seed(7)
  b <- simulate_hdr(2, 30, 20, 2, cores = 1)
  expect_identical(a, b)
})

test_that("arguments in ... reach instance_hdr", {
  lag1_cor <- function(fit, x, y, beta) cor(x[, 1], x[, 2])
  out <- simulate_hdr(3, 3000, 5, 1, x.par = 0.8, permuted = FALSE,
                      fitfun = function(x, y) NULL, postfun = lag1_cor,
                      cores = 1, seed = 1)
  expect_equal(unlist(out), rep(0.8, 3), tolerance = 0.05)
})

test_that("invalid arguments fail fast", {
  expect_error(simulate_hdr(2, 30, 20, 2, fitarg = list(), cores = 1), "fitarg")
  expect_error(simulate_hdr(2, 30, 20, 2, list(), cores = 1), "must be named")
  expect_error(simulate_hdr(0, 30, 20, 2, cores = 1), "niters")
  expect_error(simulate_hdr(2, 30, 20, 2, cores = 0), "cores")
})

test_that("parallel results match sequential results and clean up", {
  skip_on_cran()
  seq_result <- simulate_hdr(4, 50, 100, 5, cores = 1, seed = 42)
  par_result <- simulate_hdr(4, 50, 100, 5, cores = 2, seed = 42)
  expect_identical(seq_result, par_result)

  # custom components defined here must travel to the workers
  scale <- 10
  custom <- simulate_hdr(2, 30, 20, 2,
                         yfun = function(x, beta) drop(x %*% beta) * scale,
                         fitfun = function(x, y) max(abs(y)),
                         postfun = function(fit, x, y, beta) fit,
                         cores = 2, seed = 1)
  expect_true(all(unlist(custom) > 1))

  # the foreach backend is not left pointing at a stopped cluster
  expect_equal(foreach::getDoParName(), "doSEQ")
  expect_equal(unlist(foreach::`%dopar%`(foreach::foreach(i = 1:2), i)), 1:2)
})

test_that("workers find packages on library paths set in the session", {
  skip_on_cran()
  pkg_dir <- find.package("simcity")
  skip_if_not(dir.exists(file.path(pkg_dir, "Meta")), "simcity not installed")

  # Child session where the libraries are set with .libPaths() only, not via
  # environment variables that the workers would inherit.
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    sprintf(".libPaths(%s)",
            paste(deparse(c(dirname(pkg_dir), .libPaths())), collapse = "")),
    "sims <- simcity::simulate_hdr(2, 30, 20, 2, cores = 2, seed = 1)",
    "cat('iterations:', length(sims))"
  ), script)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(script),
    stdout = TRUE, stderr = TRUE,
    env = c("R_LIBS=", "R_LIBS_USER=")
  ))
  expect_true(any(grepl("iterations: 2", out)), info = paste(out, collapse = "\n"))
})
