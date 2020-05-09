#' Check if a local file exists and otherwise download it
#'
#' @param src    URL where the file should be downloaded if it does not exist
#' @param dest   local destination where the file should be stored
#'
#' @export
sm_check_download <- function(src, dest)
{
  if (!file.exists(dest))
  {
    d <- dirname(dest)
    if (!file.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
    utils::download.file(src, dest, quiet = TRUE)
  }
}
