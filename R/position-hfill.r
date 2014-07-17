#' Stack overlapping objects on top of one another, and standardise to have
#' equal height.
#' 
#' This is a modified version of position fill in ggplot2, swapping the role 
#' of 'x' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#'
#' @inheritParams ggplot2::position_identity
#' @family position adjustments
#' @author Shiv Venkatasubrahmanyam
#' @export
position_hfill <- function (width = NULL, height = NULL) { 
  PositionHFill$new(width = width, height = height)
}
  
#' @export 
PositionHFill <- proto(ggplot2:::Position, {
  objname <- "hfill"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    
    ggplot2:::check_required_aesthetics(c("y", "xmax"), names(data), "position_hfill")
    if (!all(data$xmin == 0)) warning("Filling not well defined when xmin != 0")
    hcollide(data, .$width, .$my_name(), pos_hfill)
  }  

  icon <- function(.) {
    y <- c(0.5, 0.8)
    rectGrob(0.5, c(0.625, 1), width=0.4, height=c(0.625, 0.375), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }
})
