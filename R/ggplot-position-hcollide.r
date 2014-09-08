#' Detect and prevent collisions. Powers dodging, stacking and filling.
#' 
#' This is modified version of collide in ggplot2, swapping the roles of 'x' 
#' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#' 
#' @param data
#' @param width
#' @param name
#' @param strategy
#' @param check.width
#' @author Shiv Venkatasubrahmanyam
#' @import plyr
#' @import scales
#' @export
hcollide <- function(data, width = NULL, name, strategy, check.width = TRUE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data <- within(data, {
        ymin <- y - width / 2
        ymax <- y + width / 2
      })      
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }
    
    # Width determined from data, must be floating point constant 
    widths <- unique(with(data, ymax - ymin))
    widths <- widths[!is.na(widths)]
    if (!zero_range(range(widths))) {
      warning(name, " requires constant width: output may be incorrect", 
        call. = FALSE)
    }
    width <- widths[1]
  }

  # Reorder by x position, relying on stable sort to preserve existing 
  # ordering, which may be by group or order.
  data <- data[order(data$ymin), ]

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]
  
  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots. 
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$xmax)) {
    ddply(data, .(ymin), strategy, width = width)
  } else if (!is.null(data$x)) {
    message("xmax not defined: adjusting position using x instead")
    transform(
      ddply(transform(data, xmax = x), .(ymin), strategy, width = width),
      x = xmax
    )
  } else {
    stop("Neither x nor xmax defined")
  }
}

#' Stack overlapping intervals. Assumes that each set has the same horizontal 
#' position.
#' 
#' This is modified version of pos_stack in ggplot2, swapping the roles of 'x' 
#' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#' 
#' @param df
#' @param width
#' @author Shiv Venkatasubrahmanyam
#' @export
pos_hstack <- function(df, width) {
  if (nrow(df) == 1) return(df)
  
  n <- nrow(df) + 1
  x <- with(df, ifelse(is.na(x), 0, x))
  if (all(is.na(df$y))) {
    heights <- rep(NA, n)
  } else {
    heights <- c(0, cumsum(x))
  }

  within(df, {
    xmin <- heights[-n]
    xmax <- heights[-1]
    x <- xmax
  })
}

#' Stack overlapping intervals and set height to 1. Assumes that each set has 
#' the same horizontal position.
#' 
#' This is modified version of pos_fill in ggplot2, swapping the roles of 'x' 
#' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#' 
#' @inheritParams pos_hstack
#' @author Shiv Venkatasubrahmanyam
#' @export
pos_hfill <- function(df, width) {
  within(pos_hstack(df, width), {
    xmin <- xmin / max(xmax)
    xmax <- xmax / max(xmax)
    x <- xmax
  })
}

#' Dodge overlapping interval. Assumes that each set has the same horizontal 
#' position.
#' 
#' This is modified version of pos_dodge in ggplot2, swapping the roles of 'x' 
#' and 'y', meant to be used with geom_hbar for horizontal bar graphs.
#' 
#' @inheritParams pos_hstack
#' @author Shiv Venkatasubrahmanyam
#' @export
pos_hdodge <- function(df, width) {
  n <- length(unique(df$group))
  if (n == 1) return(df)
  
  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }

  d_width <- with(df, max(ymax - ymin))    
  diff <- width - d_width
  
  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # qplot(n, div, data = df)
  
  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidy <- match(df$group, sort(unique(df$group)))

  within(df, {
    ymin <- ymin + width / n * (groupidy - 1) - diff * (n - 1) / (2 * n)
    ymax <- ymin + d_width / n
    y <- (ymin + ymax) / 2
  })
}
