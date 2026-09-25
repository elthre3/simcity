#' Summarize support recovery and estimation error across simulations
#'
#' @description
#' Computes per-iteration performance metrics from true and estimated linear
#' model coefficients, such as the output of [simulate_hdr()] with the default
#' [post_glmnet_coefs()].
#'
#' @details
#' Rows with `term == "(Intercept)"` are dropped before computing anything.
#' Let \eqn{S = \{j : \beta_j \neq 0\}} be the true support and
#' \eqn{\hat S = \{j : \hat\beta_j \neq 0\}} the estimated support over the
#' \eqn{p} slope coefficients.
#'
#' @param list_of_instances A list of data frames, each with columns
#'   `estimate` and `true_beta` (and optionally `term`).
#' @returns
#' A `data.frame` with one row per instance and columns:
#'
#' * `iter` Index of the instance in `list_of_instances`.
#' * `true_sparsity` \eqn{|S|}.
#' * `estimated_sparsity` \eqn{|\hat S|}.
#' * `true_positives` \eqn{|S \cap \hat S|}.
#' * `false_positives` \eqn{|\hat S \setminus S|}.
#' * `false_negatives` \eqn{|S \setminus \hat S|}.
#' * `fdp` False discovery proportion,
#'   \eqn{|\hat S \setminus S| / \max(1, |\hat S|)}.
#' * `screened` Whether \eqn{S \subseteq \hat S}, i.e. the selected model
#'   includes every truly nonzero coefficient.
#' * `exact_support` Whether \eqn{\hat S = S}.
#' * `mse` \eqn{\|\hat\beta - \beta\|_2^2 / p}.
#' * `beta_min` \eqn{\min_{j \in S} |\beta_j|}, or `NA` if \eqn{S} is empty.
#' @export
#' @examples
#' sims <- simulate_hdr(niters = 4, n = 50, p = 100, s0 = 5,
#'                      cores = 1, seed = 1)
#' simmary_coefs(sims)
simmary_coefs <- function(list_of_instances) {
  if (!is.list(list_of_instances) || is.data.frame(list_of_instances)) {
    stop("`list_of_instances` must be a list of data frames.", call. = FALSE)
  }
  rows <- lapply(seq_along(list_of_instances), function(i) {
    inst <- list_of_instances[[i]]
    if (!all(c("estimate", "true_beta") %in% names(inst))) {
      stop("Instance ", i, " lacks `estimate` and `true_beta` columns.",
           call. = FALSE)
    }
    if ("term" %in% names(inst)) inst <- inst[inst$term != "(Intercept)", ]
    true_support <- which(inst$true_beta != 0)
    estimated_support <- which(inst$estimate != 0)
    tp <- sum(true_support %in% estimated_support)
    fp <- length(estimated_support) - tp
    fn <- length(true_support) - tp
    data.frame(
      iter = i,
      true_sparsity = length(true_support),
      estimated_sparsity = length(estimated_support),
      true_positives = tp,
      false_positives = fp,
      false_negatives = fn,
      fdp = fp / max(1, length(estimated_support)),
      screened = fn == 0,
      exact_support = fn == 0 && fp == 0,
      mse = mean((inst$estimate - inst$true_beta)^2),
      beta_min = if (length(true_support) > 0) {
        min(abs(inst$true_beta[true_support]))
      } else {
        NA_real_
      }
    )
  })
  do.call(rbind, rows)
}
