#' Confidence interval summaries
#'
#' Summarize a variable by giving confidence interval for the mean or median.
#' These are a convenience wrappers around the Hmisc functions.
#'
#' @param x      the variable to summarize. Will be used deparsed to infer the
#'               names of of the summaries.
#' @param ...    other arguments passed on to the respective Hmisc function.
#' @param name   provide a prefix variable name for the output. Set to
#'               \code{NULL} to infer from the input.

#' @name ConfInt
NULL

#' @rdname ConfInt
#' @export
sm_mean_ci_boot <- function(x, ..., name = NULL) {
  tf <- do.call(Hmisc::smean.cl.boot, list(x = quote(x), ...))
  tf <- as.data.frame(as.list(tf))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(tf) <- sprintf(c("%s_mean", "%s_ci_min", "%s_ci_max"), cname)
  tf
}

#' @rdname ConfInt
#' @export
sm_mean_ci_normal <- function(x, ..., name = NULL) {
  tf <- do.call(Hmisc::smean.cl.normal, list(x = quote(x), ...))
  tf <- as.data.frame(as.list(tf))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(tf) <- sprintf(c("%s_mean", "%s_ci_min", "%s_ci_max"), cname)
  tf
}

#' @rdname ConfInt
#' @export
sm_median_hilow <- function(x, ..., name = NULL) {
  tf <- do.call(Hmisc::smedian.hilow, list(x = quote(x), ...))
  tf <- as.data.frame(as.list(tf))
  cname <- ifelse(is.null(name), deparse(substitute(x)), name)
  names(tf) <- sprintf(c("%s_median", "%s_ci_min", "%s_ci_max"), cname)
  tf
}
