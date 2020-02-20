#' Minimal theme perfect for presentations
#'
#' Return a minimalistic theme based on Tufte's
#' *Visual Display of Quantitative Information*, with some custom tweaks

#' @export
theme_sm <- function()
{
    ret <- ggplot2::theme_bw(base_family = "sans", base_size = 11) +
        ggplot2::theme(
          legend.background = ggplot2::element_blank(),
          legend.key        = ggplot2::element_blank(),
          panel.background  = ggplot2::element_blank(),
          panel.border      = ggplot2::element_blank(),
          strip.background  = ggplot2::element_blank(),
          plot.background   = ggplot2::element_blank(),
          axis.line         = ggplot2::element_blank(),
          panel.grid        = ggplot2::element_blank(),
          axis.ticks        = ggplot2::element_blank()
        )
    ret
}
