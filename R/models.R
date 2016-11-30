#' Fit a basic linear model
#'
#' @importFrom         stats lm
#'
#' @param formula      an object of class \code{formula} (or one that can be
#'                     coerced to that class): a symbolic description of the
#'                     model to be fitted.
#' @param data         a data_frame containing the variables in the model
#'
#' @export
lm_basic <- function(formula, data) {
  out <- lm(formula, data)
  out$call <- match.call()
  class(out) = c("lm_basic", "lm")
  out
}

