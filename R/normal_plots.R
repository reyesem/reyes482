# ggplot methods for producing probability plots

################################################################################
# fct: stat_halfnorm
# description: Make a stat for ggplot2 that makes the halfnormal plot.

StatHalfnorm <- ggplot2::ggproto("StatHalfnorm", ggplot2::Stat,
                        compute_group = function(data, scales, na.rm = FALSE){
                          # sort data
                          sample <- sort(data$sample)

                          # obtain length
                          n <- length(sample)

                          # determine quanitles
                          halfnorm <- stats::qnorm((n + seq(n))/(2*n + 1))

                          data.frame(sample, halfnorm)
                        },
                        required_aes = "sample",
                        default_aes = ggplot2::aes(x = ..halfnorm..,
                                                   y = ..sample..))

#' Halfnormal plot.
#'
#' Construct a half-normal plot.
#'
#' @param mapping see \code{\link[ggplot2]{stat_identity}}
#' @param data see \code{\link[ggplot2]{stat_identity}}
#' @param geom see \code{\link[ggplot2]{stat_identity}} (default = "point")
#' @param position see \code{\link[ggplot2]{stat_identity}} (default =
#' "identity")
#' @param na.rm boolean. If \code{TRUE}, missing values are removed (default =
#' FALSE)
#' @param show.legend see \code{\link[ggplot2]{stat_identity}}
#' @param inherit.aes see \code{\link[ggplot2]{stat_identity}}
#' @param ... see \code{\link[ggplot2]{stat_identity}}
#'
#' @export
stat_halfnorm <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = StatHalfnorm,
                 geom = geom,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params=list(na.rm = na.rm, ...))
}

