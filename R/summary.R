#' Numeric Summaries
#'
#' Summarize a variable through a single numeric summary, such as the mean
#' standard deviation, and sum.
#'
#' @param x      the variable to summarize. Will be used deparsed to infer the
#'               names of of the summaries.
#' @param y      secomd variable to summarize, for correlation, covariance, and
#'               variance.
#' @param name   provide a prefix variable name for the output. Set to
#'               \code{NULL} to infer from the input.
#' @param collapse   string to use to collapse text values with `sm_collapse`
#' @name SingleSummary

#' @rdname SingleSummary
#' @export
sm_mean <- function(x, name = NULL) {
  res <- as.data.frame(mean(x, na.rm = TRUE))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_mean", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_median <- function(x, name = NULL) {
  res <- as.data.frame(stats::median(x, na.rm = TRUE))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_median", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_sd <- function(x, name = NULL) {
  res <- as.data.frame(stats::sd(x, na.rm = TRUE))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_sd", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_sum <- function(x, name = NULL) {
  res <- as.data.frame(sum(x, na.rm = TRUE))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_sum", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_min <- function(x, name = NULL) {
  res <- as.data.frame(min(x, na.rm = TRUE))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_min", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_max <- function(x, name = NULL) {
  res <- as.data.frame(max(x, na.rm = TRUE))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_max", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_cor <- function(x, y, name = NULL) {
  res <- as.data.frame(stats::cor(x, y, use = "pairwise.complete.obs"))
  if (is.null(name))
  {
    name <- sprintf("%s_%s", deparse(substitute(x)), deparse(substitute(y)))
  }
  names(res) <- sprintf("%s_cor", name)
  res
}

#' @rdname SingleSummary
#' @export
sm_cov <- function(x, y, name = NULL) {
  res <- as.data.frame(stats::cov(x, y, use = "pairwise.complete.obs"))
  if (is.null(name))
  {
    name <- sprintf("%s_%s", deparse(substitute(x)), deparse(substitute(y)))
  }
  names(res) <- sprintf("%s_cov", name)
  res
}

#' @rdname SingleSummary
#' @export
sm_var <- function(x, y, name = NULL) {
  res <- as.data.frame(stats::var(x, y, na.rm = TRUE))
  if (is.null(name))
  {
    name <- sprintf("%s_%s", deparse(substitute(x)), deparse(substitute(y)))
  }
  names(res) <- sprintf("%s_var", name)
  res
}

#' @rdname SingleSummary
#' @export
sm_na_count <- function(x, name = NULL) {
  res <- as.data.frame(sum(is.na(x)))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_na_count", cname)
  res
}

#' @rdname SingleSummary
#' @export
sm_count <- function(x, name = NULL) {
  res <- as.data.frame(dplyr::n())
  cname <- ifelse(is.null(name), "count", name)
  names(res) <- cname
  res
}

#' @rdname SingleSummary
#' @export
sm_collapse <- function(x, name = NULL, collapse = "; ") {
  res <- as.data.frame(paste(x, collapse = collapse))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(res) <- sprintf("%s_collapse", cname)
  res
}
