#' A variant geom_bar (ggplot2) that plots horizontal bars
#' 
#' This is useful in situations where geom_bar() + coord_flip() doesn't work.
#' For instance, geom_bar() + facet_wrap(..., scales = "free") + coord_flip()
#' is not supported (see https://github.com/hadley/ggplot2/issues/45).
#' 
#' Currently only the "identity" stat and position are supported (but not 
#' enforced, so specify alternatives at your own risk). Adding this 
#' support would require creating the appropriate variants of Stat* and 
#' Position* classes. Existing stats can be used provided they are supplied 
#' with a mapping that swaps x and y. I'm not aware of any such quick fix for 
#' positions.
#' 
#' @inheritParams ggplot2::geom_point
#' @import ggplot2 grid proto
#' @author Shiv Venkatasubrahmanyam
#' @export 
geom_hbar <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  if (is.character(position) && position %in% c("stack", "fill", "dodge"))
    position <- paste("h", position, sep = "")
#  position <- c(
#    "identity" = "identity", 
#    "jitter"   = "jitter", 
#    "stack"    = "hstack", 
#    "fill"     = "hfill", 
#    "dodge"    = "hdodge", 
#    "hstack"    = "hstack", 
#    "hfill"     = "hfill", 
#    "hdodge"    = "hdodge")[position]
  GeomHBar$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

#' A modified version of GeomBar (ggplot2) replacing x with y as needed
#' 
#' @keywords internal
GeomHBar <- proto(ggplot2:::Geom, {
    objname <- "hbar"
    
    default_stat <- function(.) ggplot2:::StatIdentity
    default_pos <- function(.) ggplot2:::PositionIdentity
    default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, weight = 1, alpha = 1)
    
    required_aes <- c("y")
    
    reparameterise <- function(., df, params) {
      df$width <- df$width %||% 
        params$width %||% (resolution(df$y, FALSE) * 0.9)
      transform(df,
        xmin = pmin(x, 0), xmax = pmax(x, 0),
        ymin = y - width / 2, ymax = y + width / 2, width = NULL
      )
    }
    
    draw_groups <- function(., data, scales, coordinates, ...) {
      ggplot2:::GeomRect$draw_groups(data, scales, coordinates, ...)
    }
    guide_geom <- function(.) "polygon"
    
    
    icon <- function(.) {
      rectGrob(c(0.3, 0.7), c(0.4, 0.8), height=c(0.4, 0.8), width=0.3, vjust=1, gp=gpar(fill="grey20", col=NA))
    }
  })
