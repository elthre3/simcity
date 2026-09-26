check_count <- function(x, name, min = 0) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x != round(x) || x < min) {
    stop("`", name, "` must be a single integer >= ", min, ".", call. = FALSE)
  }
  invisible(x)
}

check_dims <- function(n, p, s0) {
  check_count(n, "n", min = 1)
  check_count(p, "p", min = 1)
  check_count(s0, "s0", min = 0)
  if (s0 > p) stop("`s0` must be at most `p`.", call. = FALSE)
  invisible(TRUE)
}

check_component <- function(fun, args, fun_name, args_name) {
  if (!is.function(fun)) {
    stop("`", fun_name, "` must be a function.", call. = FALSE)
  }
  if (!is.list(args)) {
    stop("`", args_name, "` must be a list.", call. = FALSE)
  }
  if (length(args) > 0 && (is.null(names(args)) || any(names(args) == ""))) {
    stop("All elements of `", args_name, "` must be named.", call. = FALSE)
  }
  invisible(TRUE)
}
