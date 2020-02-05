#' Quantile summaries
#'
#' Summarize a variable through its quantiles.
#'
#' @param x      the variable to summarize. Will be used deparsed to infer the
#'               names of of the summaries.
#' @param probs  a complete list of probabilites to compute when calling the
#'               \code{sm_quantiles} function.
#' @param name   provide a prefix variable name for the output. Set to
#'               \code{NULL} to infer from the input.

#' @name QuantileSummary

#' @rdname QuantileSummary
#' @export
sm_quartiles <- function(x, name = NULL) {
  res <- stats::quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE)
  res <- as.data.frame(as.list(res))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_%s", cname, c("min", "q1", "median", "q3", "max"))
  res
}

#' @rdname QuantileSummary
#' @export
sm_deciles <- function(x, name = NULL) {
  res <- stats::quantile(x, probs = seq(0, 1, 0.1), na.rm = TRUE)
  res <- as.data.frame(as.list(res))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_d%d", cname, seq(0, 10, by = 1))
  res
}

#' @rdname QuantileSummary
#' @export
sm_ventiles <- function(x, name = NULL) {
  res <- stats::quantile(x, probs = seq(0, 1, 0.05), na.rm = TRUE)
  res <- as.data.frame(as.list(res))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_v%d", cname, seq(0, 20, by = 1))
  res
}

#' @rdname QuantileSummary
#' @export
sm_percentiles <- function(x, name = NULL) {
  res <- stats::quantile(x, probs = seq(0, 1, 0.01), na.rm = TRUE)
  res <- as.data.frame(as.list(res))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_p%d", cname, seq(0, 100, by = 1))
  res
}

#' @rdname QuantileSummary
#' @export
sm_quantiles <- function(x, probs = seq(0, 1, 0.25), name = NULL) {
  res <- stats::quantile(x, probs = probs, na.rm = TRUE)
  res <- as.data.frame(as.list(res))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_%s", names(res), cname)
  res
}
