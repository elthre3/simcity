#' Repeat a high-dimensional regression simulation in parallel
#'
#' @description
#' Runs [instance_hdr()] `niters` times, in parallel when `cores > 1`, with
#' reproducible random number streams.
#'
#' @details
#' Iterations are run with [doRNG::%dorng%], which gives
#' each iteration its own L'Ecuyer-CMRG random number stream. Results for a
#' given `seed` are therefore identical whatever the value of `cores`. The
#' per-iteration seeds are stored in `attr(result, "rng")`. Set
#' `.Random.seed` to one of them to re-run a single iteration.
#'
#' With `cores > 1`, a PSOCK cluster is created and stopped on exit. On exit
#' the foreach backend is reset to sequential ([foreach::registerDoSEQ()]),
#' so a backend you registered before the call is not restored.
#'
#' Workers are separate R processes. Custom components that call functions
#' from other packages should use `pkg::fun()`, or list those packages in
#' `packages`. With `cores > 1`, simcity itself must be installed, not just
#' loaded with `devtools::load_all()`.
#'
#' @param niters Number of simulation iterations.
#' @param n,p,s0 Sample size, number of predictors, and sparsity. See
#'   [instance_hdr()].
#' @param ... Further named arguments passed to [instance_hdr()], such as
#'   `xtype`, `btype`, `x.par`, `yfun`, `fitargs`.
#' @param cores Number of worker processes. Defaults to half of
#'   [parallel::detectCores()], at least 1 and at most `niters`. `cores = 1`
#'   runs sequentially in the current session.
#' @param seed Optional integer seed. If `NULL`, the streams are derived from
#'   the current RNG state, so a preceding [set.seed()] also gives
#'   reproducible results.
#' @param packages Character vector of packages to load on each worker.
#' @param verbose If `TRUE`, report the elapsed time.
#' @return A list of length `niters` with the output of [instance_hdr()] for
#'   each iteration.
#' @seealso [simmary_coefs()]
#' @export
#' @examples
#' sims <- simulate_hdr(niters = 4, n = 50, p = 100, s0 = 5,
#'                      cores = 1, seed = 1)
#' simmary_coefs(sims)
#'
#' \donttest{
#' # Elastic net on an equicorrelated design, using 2 cores
#' sims <- simulate_hdr(20, 100, 200, 10,
#'                      xtype = "equi.corr", x.par = 0.5,
#'                      fitargs = list(alpha = 0.5),
#'                      cores = 2, seed = 1)
#' }
simulate_hdr <- function(
    niters,
    n,
    p,
    s0,
    ...,
    cores = NULL,
    seed = NULL,
    packages = NULL,
    verbose = FALSE
)
{
  check_count(niters, "niters", min = 1)
  check_dims(n, p, s0)
  instance_args <- c(list(n = n, p = p, s0 = s0), check_dots(...))
  cores <- resolve_cores(cores, niters)

  time_start <- Sys.time()
  on.exit(foreach::registerDoSEQ(), add = TRUE)
  if (cores > 1L) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doParallel::registerDoParallel(cl)
  } else {
    foreach::registerDoSEQ()
  }

  output <- foreach::foreach(
    i = seq_len(niters),
    .packages = packages,
    .options.RNG = seed
  ) %dorng% {
    do.call(instance_hdr, instance_args)
  }

  if (verbose) {
    elapsed <- Sys.time() - time_start
    message(sprintf("simulate_hdr: %d iterations on %d core(s) in %.1f %s",
                    niters, cores, elapsed, units(elapsed)))
  }
  output
}

resolve_cores <- function(cores, niters) {
  if (is.null(cores)) {
    detected <- parallel::detectCores()
    if (is.na(detected)) detected <- 1L
    cores <- floor(detected / 2)
  } else {
    check_count(cores, "cores", min = 1)
  }
  as.integer(max(1L, min(cores, niters)))
}

check_dots <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) return(dots)
  dot_names <- names(dots)
  if (is.null(dot_names) || any(dot_names == "")) {
    stop("All arguments in `...` must be named.", call. = FALSE)
  }
  allowed <- setdiff(names(formals(instance_hdr)), c("n", "p", "s0"))
  unknown <- setdiff(dot_names, allowed)
  if (length(unknown) > 0) {
    stop("Unknown argument(s) for instance_hdr(): ",
         paste0("`", unknown, "`", collapse = ", "), call. = FALSE)
  }
  dots
}
