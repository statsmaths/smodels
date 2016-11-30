#' Summarize a data set by group or groups
#'
#' @importFrom         stats median sd
#' @importFrom         lazyeval lazy_dots
#' @importFrom         dplyr group_by_ select_if summarize_all summarize ungroup funs n
#'
#' @param .data    a data_frame containing the variables in the model
#' @param ...     variables to group by.
#'
#' @export
group_summarize <- function(.data, ...) {

  data <- group_by_(.data, .dots = lazyeval::lazy_dots(...))
  group_vars <- sapply(attributes(data)$vars, as.character)

  data <- select_if(data, is.numeric)
  results <- summarize_all(data, funs(mean, median, sd, sum))

  if (ncol(data) <= length(group_vars) + 1) {
    these <- (names(results) %in% group_vars)
    names(results)[!these] <- paste0(setdiff(names(data),group_vars), "_", names(results)[!these])
  }

  results$n <- summarize(data, n = n())$n
  ungroup(results)

}

#' Count number of elements in each group.
#'
#' @importFrom         stats median sd
#' @importFrom         lazyeval lazy_dots
#' @importFrom         dplyr group_by_ select_if summarize_all summarize ungroup funs n arrange desc
#'
#' @param .data   a data_frame containing the variables in the model
#' @param ...     variables to group by.
#' @param sort    logical. Should the data be sorted on output.
#'
#' @export
group_count <- function(.data, ..., sort = FALSE) {

  data <- group_by_(.data, .dots = lazyeval::lazy_dots(...))
  group_vars <- sapply(attributes(data)$vars, as.character)

  data <- select_if(data, is.numeric)
  results <- summarize(data, n = n())

  results <- ungroup(results)
  if (sort) {
    results <- arrange(results, desc(n))
  }
  results
}

#' Add predictions from a fitted model to a data set
#'
#' @importFrom         stats resid predict
#'
#' @param data       dataset to add predictions to
#' @param object      a model object
#'
#' @export
add_prediction <- function(data, object){
  pred <- predict(object, newdata = data)
  resid <- resid(object, newdata = data)

  model_name <- deparse(substitute(object))

  data[,paste(model_name,"pred",sep="_")] <- pred
  data[,paste(model_name,"resid",sep="_")] <- resid

  data
}




