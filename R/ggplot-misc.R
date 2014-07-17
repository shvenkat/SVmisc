#' Extract ggplot legend
#' 
#' @param x ggplot object
#' @return grob that can be used to render the legend
#' @author Shiv Venkatasubrahmanyam
#' @import ggplot2
#' @export
getGgplotLegend <- function(x) {
  tmp <- ggplot_gtable(ggplot_build(x))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
