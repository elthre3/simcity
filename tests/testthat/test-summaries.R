test_that("simmary_coefs counts are exact and ignore the intercept", {
  inst <- data.frame(
    term = c("(Intercept)", paste0("V", 1:6)),
    estimate = c(0.3, 1.1, 0, 0.2, 0.5, 0, 0),
    true_beta = c(0, 1, 1, 0, 0, 0, 0)
  )
  out <- simmary_coefs(list(inst, inst))
  expect_equal(nrow(out), 2)
  expect_equal(out$iter, 1:2)
  row <- out[1, ]
  expect_equal(row$true_sparsity, 2)
  expect_equal(row$estimated_sparsity, 3)
  expect_equal(row$true_positives, 1)
  expect_equal(row$false_positives, 2)
  expect_equal(row$false_negatives, 1)
  expect_equal(row$fdp, 2 / 3)
  expect_false(row$screened)
  expect_false(row$exact_support)
  expect_equal(row$mse, mean(c(0.1, 1, 0.2, 0.5, 0, 0)^2))
  expect_equal(row$beta_min, 1)
})

test_that("simmary_coefs handles perfect recovery and empty supports", {
  perfect <- data.frame(estimate = c(2, 0, 0), true_beta = c(2, 0, 0))
  empty <- data.frame(estimate = c(0, 0), true_beta = c(0, 0))
  out <- simmary_coefs(list(perfect, empty))
  expect_true(out$screened[1])
  expect_true(out$exact_support[1])
  expect_equal(out$fdp, c(0, 0))
  expect_true(is.na(out$beta_min[2]))
  expect_error(simmary_coefs(perfect), "list of data frames")
  expect_error(simmary_coefs(list(data.frame(a = 1))), "lacks")
})
