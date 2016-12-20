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
#' @param data        dataset to add predictions to
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

#' Map points to quantiles
#'
#' @importFrom         stats quantile
#'
#' @param x   data to bin
#' @param n   number of buckets
#'
#' @export
bin <- function(x, n) {

  breaks <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  cut(x, breaks, include.lowest = TRUE)

}

#' A smarter map function
#'
#' @importFrom         ggmap ggmap get_googlemap
#' @importFrom         ggplot2 theme element_blank
#'
#' @param x        variable name that defines the longitude
#' @param y        variable name that defines the latitude
#' @param ...      other arguments passed to qplot
#' @param data     the data frame containing longitude and latitude
#' @param maptype  the type of map; either "toner" or "road"
#' @param zoom     factor giving how much more area the map should cover than
#'                 the minimum area needed for the plot.
#' @param detail   integer giving how much more detail should be given to the
#'                 map tiles compared to the default. Not applicable to road
#'                 maps. Negative integers are allowed, but are rarely useful.
#' @param location an option string describing the location to center the box on.
#'
#' @export
mplot <- function(x, y, ..., data, maptype = c("toner", "road"), zoom = 1,
                  detail = 0, location = NULL) {
  if (missing(x))
    stop("You must supply a longitude")
  if (missing(y))
    stop("You must supply a latitude")

  detail <- as.integer(detail)

  # Get map range and zoom level:
  maptype <- match.arg(maptype)
  if (is.null(location)) {
    lon_range <- range(data[[deparse(substitute(x))]])
    lat_range <- range(data[[deparse(substitute(y))]])
  } else {
    gm <- ggmap::get_googlemap(center = location)
    location <- as.numeric(attr(gm, "bb"))[c(2, 1, 4, 3)]
    lon_range <- location[c(1, 3)]
    lat_range <- location[c(2, 4)]
  }

  # Change range based on zoom:
  lon_mean <- mean(lon_range)
  lon_delta <- diff(lon_range) / 2
  lon_range <- lon_mean + c(-1, 1) * lon_delta * sqrt(zoom)
  lat_mean <- mean(lat_range)
  lat_delta <- diff(lat_range) / 2
  lat_range <- lat_mean + c(-1, 1) * lat_delta * sqrt(zoom)

  # grab aesthetics
  argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  env <- parent.frame()
  aesthetics <- .compact(arguments[.all_aesthetics])
  aesthetics <- aesthetics[!.is.constant(aesthetics)]
  aes_names <- names(aesthetics)
  aesthetics <- .rename_aes(aesthetics)
  class(aesthetics) <- "uneval"

  # Load the map
  my_map <- .get_map(lon_range, lat_range, maptype, detail)

  z <- ggmap(my_map)
  z[["data"]] <- data
  z[["mapping"]] <- aesthetics
  z <- z + theme(axis.line=element_blank(), axis.text.x=element_blank(),
            axis.text.y=element_blank(), axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  z
}

#' @importFrom         ggmap OSM_scale_lookup get_map
.get_map <- function(lon_range, lat_range, maptype, detail) {
  # calculate zoom and scale:
  lonlength <- diff(lon_range)
  latlength <- diff(lat_range)
  zoomlon <- ceiling(log2(360 * 2/lonlength))
  zoomlat <- ceiling(log2(180 * 2/latlength))
  zoom <- max(zoomlon, zoomlat)

  zoom <- zoom + detail

  if (maptype == "toner") {
    source <- "stamen"
    maptype <- "toner"
    scale <- ggmap::OSM_scale_lookup(zoom)
  } else if (maptype == "road") {
    source <- "google"
    maptype <- "roadmap"
    scale <- 2
  }

  my_map <- ggmap::get_map(location = c(lon_range[1], lat_range[1], lon_range[2], lat_range[2]),
                    source = source, maptype = maptype, crop = TRUE, force = FALSE,
                    scale = scale, zoom = zoom)
  my_map
}

#' Calculate deciles
#'
#' @param x        values to compute deciles for
#'
#' @importFrom         stats quantile
#' @export
deciles <- function(x) {
  stats::quantile(x, seq(0, 1, 0.1))
}

#' Calculate ventiles
#'
#' @param x        values to compute ventiles for
#'
#' @importFrom         stats quantile
#' @export
ventiles <- function(x) {
  stats::quantile(x, seq(0, 1, 0.05))
}

#' Calculate quartiles
#'
#' @param x        values to compute quartiles for
#'
#' @importFrom         stats quantile
#' @export
quartiles <- function(x) {
  stats::quantile(x, seq(0, 1, 0.25))
}

#' Calculate quartiles
#'
#' @param x        values to compute quartiles for
#'
#' @importFrom         stats quantile
#' @export
percentiles <- function(x) {
  stats::quantile(x, seq(0, 1, 0.01))
}


.compact <- function (x) Filter(Negate(is.null), x)

.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour",
"fg", "fill", "group", "hjust", "label", "linetype", "lower",
"lty", "lwd", "max", "middle", "min", "pch", "radius", "sample",
"shape", "size", "srt", "upper", "vjust", "weight", "width",
"x", "xend", "xmax", "xmin", "xintercept", "y", "yend", "ymax",
"ymin", "yintercept", "z")

.base_to_ggplot <- structure(c("colour", "colour", "shape", "size", "linetype",
"size", "angle", "hjust", "fill", "colour", "ymin", "ymax"), .Names = c("col",
"color", "pch", "cex", "lty", "lwd", "srt", "adj", "bg", "fg",
"min", "max"))

.is.constant <- function (x)
{
    is_I_call <- function(x) is.call(x) && identical(x[[1]],
        quote(I))
    vapply(x, is_I_call, logical(1))
}

#' @importFrom         plyr rename
.rename_aes <- function (x)
{
    full <- match(names(x), .all_aesthetics)
    names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]
    plyr::rename(x, .base_to_ggplot, warn_missing = FALSE)
}
