#' Plot group means
#'
#' @importFrom         ggplot2 geom_point
#'
#' @param ...        Parameters passed to the geometry.
#'
#' @export
geom_mean <- function(...) {
    geom_point(stat = "summary", fun.y = "mean", ...)
}


#' Plot group confidence intervals
#'
#' @importFrom         ggplot2 stat_summary
#'
#' @param ...        Parameters passed to the geometry.
#'
#' @export
geom_confint <- function(...) {
    stat_summary(fun.data=mean_cl_normal, ...)
}

#' Plot best fit line 
#'
#' @importFrom         ggplot2 geom_smooth
#'
#' @param ...        Parameters passed to the geometry.
#'
#' @export
geom_bestfit <- function(...) {
    geom_smooth(method = "lm", ...)
}   

