#' Extract ggplot legend
#'
#' @param x ggplot object
#' @return grob that can be used to render the legend
#' @import ggplot2
#' @export
ggLegend <- function(x) {
  tmp <- ggplotGrob(x)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
