#' Stack overlapping objects on top of one another.
#' 
#' This is a modified version of position stack in ggplot2, swapping the role 
#' of 'x' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#'
#' @inheritParams ggplot2::position_identity
#' @family position adjustments
#' @author Shiv Venkatasubrahmanyam
#' @export
position_hstack <- function (width = NULL, height = NULL) { 
  PositionHStack$new(width = width, height = height)
}

#' @export 
PositionHStack <- proto(ggplot2:::Position, {
  objname <- "hstack"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())

    data <- remove_missing(data, FALSE, 
      c("y", "x", "xmin", "xmax", "ymin", "ymax"), name = "position_hstack")
    
    if (is.null(data$xmax) && is.null(data$x)) {
      message("Missing x and xmax in position = 'hstack'. ", 
        "Maybe you want position = 'identity'?")
      return(data)
    }

    if (!is.null(data$xmin) && !all(data$xmin == 0)) 
      warning("Stacking not well defined when xmin != 0", call. = FALSE)

    hcollide(data, .$width, .$my_name(), pos_hstack)
  }  
  
  icon <- function(.) {
    y <- c(0.5, 0.8)
    rectGrob(0.5, c(0.5, 0.8), width=0.4, height=c(0.5, 0.3), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }
})
