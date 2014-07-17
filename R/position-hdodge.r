#' Adjust position by dodging overlaps to the side.
#' 
#' This is a modified version of position dodge in ggplot2, swapping the role 
#' of 'x' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#'
#' @inheritParams ggplot2::position_identity
#' @family position adjustments
#' @author Shiv Venkatasubrahmanyam
#' @export
position_hdodge <- function (width = NULL, height = NULL) { 
  PositionHDodge$new(width = width, height = height)
}

#' @export 
PositionHDodge <- proto(ggplot2:::Position, {
  objname <- "hdodge"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    ggplot2:::check_required_aesthetics("y", names(data), "position_hdodge")
    
    hcollide(data, .$width, .$my_name(), pos_hdodge, check.width = FALSE)
  }  

  icon <- function(.) {
    y <- c(0.5, 0.3)
    rectGrob(c(0.25, 0.75), y, width=0.4, height=y, gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }

})

