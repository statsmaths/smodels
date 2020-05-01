#' Unravel Nested Dataset of Images into Pixels
#'
#' @param  object         the input data frame
#' @param  img_col_name   name of the column containing the image arrays
#'
#' @return  a tibble in wide format with term frequencies and tf-idf values.
#'
#' @export
sm_to_pixel <- function(object, img_col_name = "img")
{
  assert(img_col_name %in% colnames(object))
  img_cid <- which(img_col_name == colnames(object))

  df <- dplyr::bind_rows(mapply(function(u, v) {
    df <- tibble::tibble(
        id = u,
        row = as.numeric(row(v[,,1])),
        col = as.numeric(col(v[,,1])),
        nrow = dim(v)[1],
        ncol = dim(v)[2],
        red = as.numeric(v[,,1]),
        green = as.numeric(v[,,2]),
        blue = as.numeric(v[,,3]),
        alpha = as.numeric(v[,,4])
      )
    df
  }, seq_len(nrow(object)), object[[img_cid]], SIMPLIFY = FALSE))
  df <- dplyr::bind_cols(object[df$id,-img_cid], df[,seq(2, ncol(df))])

  hsv <- grDevices::rgb2hsv(df$red, df$green, df$blue, maxColorValue = 1)
  df$hue <- hsv[1,]
  df$saturation <- hsv[2,]
  df$value <- hsv[3,]
  df$hex <- grDevices::rgb(df$red, df$green, df$blue, 1)
  df
}
