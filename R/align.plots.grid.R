#' Align ggplots in a grid.
#'
#' @examples
#' \donttest{
#' grid_layout <- grid.layout( nrow=2, ncol=1, widths=c(1,1), heights=c(.8, .2) )
#' grid.newpage()
#' pushViewport( layout=grid_layout )
#' align.plots( grid_layout, list( chrt1, row1, col1 ), list( chrt2, row2, col2 ) )
#'
#' temp.gg <-
#'   ggplot(melt(ex,
#'       id.vars="Time",
#'       measure.vars=c("Temperature 1","Temperature 2")),
#'     aes(x=Time, y=value)) +
#'   geom_point(aes(colour=variable))
#' source.gg <-
#'   ggplot(ex, aes(x=Time, y=sources)) + geom_step()
#' status.gg <-
#'   ggplot(ex, aes(x=Time, y=status)) + geom_point()
#' grid_layout <- grid.layout( nrow=3, ncol=2, widths=c(5,8), heights=c(8, 5, 3) )
#' grid.newpage()
#' pushViewport( viewport( layout=grid_layout ) )
#' align.plots( grid_layout,
#'   list( temp.gg, 1, 1:2 ),
#'   list( source.gg, 2, 1 ),
#'   list( status.gg, 3, 1 ),
#'   list( source.gg, 2:3, 2 ) )
#' }
#'
#' @param gl
#'     grid layout, as returned by grid.layout. Should already have been pushed
#'     using pushViewport(viewport(layout = gl))
#' @param ... lists, each having three elements: ggplot, row, col. rows and
#'     cols are integer vectors indicating the row(s) and col(s) spanned by
#'     ggplot.
#' @return none, function is called for its side effect, plotting
#' @author Baptiste Auguie, author of ggExtra::align.plots
#' @author Harish, modified ggExtra::align.plots to support gridding, quoted at
#'     http://groups.google.com/group/ggplot2/browse_thread/thread/1b859d6b4b441c90
#' @author FNan, patched align.plots to allow null grobs, see posting at
#'     http://stackoverflow.com/questions/3305613/using-grid-and-ggplot2-to-create-join-plots-using-r
#' @author Shiv Venkatasubrahmanyam, extending above
#'     work
#' @references
#'     Code and discussion
#'     http://groups.google.com/group/ggplot2/browse_thread/thread/1b859d6b4b441c90
#'     Patch to allow null grobs
#'     http://stackoverflow.com/questions/3305613/using-grid-and-ggplot2-to-create-join-plots-using-r
#' @import ggplot2 grid
#' @export
align.plots.grid <- function(gl, ...){
  # Adopted from http://ggextra.googlecode.com/svn/trunk/R/align.r
  # BUGBUG: Does not align horizontally when one has a title.
  #    There seems to be a spacer used when a title is present.  Include the
  #    size of the spacer.  Not sure how to do this yet.
  stats.row <- vector( "list", gl$nrow )
  stats.col <- vector( "list", gl$ncol )
  lstAll <- list(...)
  dots <- lapply(lstAll, function(.g) ggplotGrob(.g[[1]]))
#  ytitles <- lapply(dots, function(.g) editGrob(getGrob(.g,"axis.title.y.text",grep=TRUE), vp=NULL))
  ytitles <- lapply(dots, function(.g) if(!is.null(getGrob(.g,"axis.title.y.text",grep=TRUE)))
        editGrob(getGrob(.g,"axis.title.y.text",grep=TRUE), vp=NULL) else ggplot2:::.zeroGrob)
#  ylabels <- lapply(dots, function(.g) editGrob(getGrob(.g,"axis.text.y.text",grep=TRUE), vp=NULL))
  ylabels <- lapply(dots, function(.g) if(!is.null(getGrob(.g,"axis.text.y.text",grep=TRUE)))
        editGrob(getGrob(.g,"axis.text.y.text",grep=TRUE), vp=NULL) else ggplot2:::.zeroGrob)
#  xtitles <- lapply(dots, function(.g) editGrob(getGrob(.g,"axis.title.x.text",grep=TRUE), vp=NULL))
  xtitles <- lapply(dots, function(.g) if(!is.null(getGrob(.g,"axis.title.x.text",grep=TRUE)))
        editGrob(getGrob(.g,"axis.title.x.text",grep=TRUE), vp=NULL) else ggplot2:::.zeroGrob)
#  xlabels <- lapply(dots, function(.g) editGrob(getGrob(.g,"axis.text.x.text",grep=TRUE), vp=NULL))
  xlabels <- lapply(dots, function(.g) if(!is.null(getGrob(.g,"axis.text.x.text",grep=TRUE)))
        editGrob(getGrob(.g,"axis.text.x.text",grep=TRUE), vp=NULL) else ggplot2:::.zeroGrob)
  plottitles <- lapply(dots, function(.g) if(!is.null(getGrob(.g,"plot.title.text",grep=TRUE)))
      editGrob(getGrob(.g,"plot.title.text",grep=TRUE), vp=NULL) else ggplot2:::.zeroGrob)
  legends <- lapply(dots, function(.g) if(!is.null(.g$children$legends))
        editGrob(.g$children$legends, vp=NULL) else ggplot2:::.zeroGrob)
  widths.left <- mapply(`+`, e1=lapply(ytitles, grobWidth),
    e2= lapply(ylabels, grobWidth), SIMPLIFY=FALSE)
  widths.right <- lapply(legends, grobWidth)
#   heights.top <- lapply(plottitles, grobHeight)
  heights.top <- lapply( plottitles, function(x) unit(0,"cm") )
  heights.bottom <- mapply(`+`, e1=lapply(xtitles, grobHeight),
    e2= lapply(xlabels, grobHeight), SIMPLIFY=FALSE)
  for ( i in seq_along( lstAll ) ) {
    lstCur <- lstAll[[i]]
    # Left
    valNew <- widths.left[[ i ]]
    valOld <- stats.col[[ min(lstCur[[3]]) ]]$widths.left.max
    if ( is.null( valOld ) ) valOld <- unit( 0, "cm" )
    stats.col[[ min(lstCur[[3]]) ]]$widths.left.max <- max( do.call( unit.c, list(valOld, valNew) ) )
    # Right
    valNew <- widths.right[[ i ]]
    valOld <- stats.col[[ max(lstCur[[3]]) ]]$widths.right.max
    if ( is.null( valOld ) ) valOld <- unit( 0, "cm" )
    stats.col[[ max(lstCur[[3]]) ]]$widths.right.max <- max( do.call( unit.c, list(valOld, valNew) ) )
    # Top
    valNew <- heights.top[[ i ]]
    valOld <- stats.row[[ min(lstCur[[2]]) ]]$heights.top.max
    if ( is.null( valOld ) ) valOld <- unit( 0, "cm" )
    stats.row[[ min(lstCur[[2]]) ]]$heights.top.max <- max( do.call( unit.c, list(valOld, valNew) ) )
    # Bottom
    valNew <- heights.bottom[[ i ]]
    valOld <- stats.row[[ max(lstCur[[2]]) ]]$heights.bottom.max
    if ( is.null( valOld ) ) valOld <- unit( 0, "cm" )
    stats.row[[ max(lstCur[[2]]) ]]$heights.bottom.max <- max( do.call( unit.c, list(valOld, valNew) ) )
  }
  for(i in seq_along(dots)){
    lstCur <- lstAll[[i]]
    nWidthLeftMax <- stats.col[[ min( lstCur[[ 3 ]] ) ]]$widths.left.max
    nWidthRightMax <- stats.col[[ max( lstCur[[ 3 ]] ) ]]$widths.right.max
    nHeightTopMax <- stats.row[[ min( lstCur[[ 2 ]] ) ]]$heights.top.max
    nHeightBottomMax <- stats.row[[ max( lstCur[[ 2 ]] ) ]]$heights.bottom.max
    pushViewport( viewport( layout.pos.row=lstCur[[2]],
        layout.pos.col=lstCur[[3]], just=c("left","top") ) )
    pushViewport(viewport(
        x=unit(0, "npc") + nWidthLeftMax - widths.left[[i]],
        y=unit(0, "npc") + nHeightBottomMax - heights.bottom[[i]],
        width=unit(1, "npc") - nWidthLeftMax + widths.left[[i]] -
          nWidthRightMax + widths.right[[i]],
        height=unit(1, "npc") - nHeightBottomMax + heights.bottom[[i]] -
          nHeightTopMax + heights.top[[i]],
        just=c("left","bottom")))
    grid.draw(dots[[i]])
    upViewport(2)
  }
}

# Hack to support faceted plots
# See http://groups.google.com/group/ggplot2/browse_thread/thread/753bd02a945a0e93
# "I did a hack where i located that # of y axis titles as in:
#   getGrob(.g,"axis.text.y.text",grep=TRUE, global=TRUE)
# If the # of grobs found > 1, assumed was vertically facetted and added in an estimate of the decoration size (I could not find the grob for it) to be 15pt.   With different fonts or other options, obviously would not work."
# - Jonathan Shore

#' Combine two _like_ plots in a horizontal arrangement with matching heights
#'
#' Modified from gtable:::cbind_gtable, which currently fails to compare some
#' types of compound units.
#' 
#' @param x
#'     a gtable, as returned by ggplotGrob
#' @param y
#'     a gtable, as returned by ggplotGrob, with the same number of rows as x
#' @return
#'     a gtable, with y appended to the right of x and plot heights scaled to
#'     match
#' @author Baptiste Auguie
#' @references http://stackoverflow.com/a/13295880
#' @import grid
#' @export
cbind_gtable_max <- function (x, y)
{
  stopifnot(nrow(x) == nrow(y))
  if (ncol(x) == 0)
    return(y)
  if (ncol(y) == 0)
    return(x)
  y$layout$l <- y$layout$l + ncol(x)
  y$layout$r <- y$layout$r + ncol(x)
  x$layout <- rbind(x$layout, y$layout)

  x$widths <- gtable:::insert.unit(x$widths, y$widths)
  x$colnames <- c(x$colnames, y$colnames)

  x$heights <- grid::unit.pmax(x$heights, y$heights)
  x$grobs <- append(x$grobs, y$grobs)
  x
}

#' Combine two _like_ plots in a vertical arrangement with matching widths
#'
#' Modified from gtable:::rbind_gtable, which currently fails to compare some
#' types of compound units.
#' 
#' @param x
#'     a gtable, as returned by ggplotGrob
#' @param y
#'     a gtable, as returned by ggplotGrob, with the same number of columns as
#'     x
#' @return
#'     a gtable, with y appended below x and plot widths scaled to match
#' @author Baptiste Auguie
#' @references http://stackoverflow.com/a/13295880
#' @import grid
#' @export
rbind_gtable_max <- function (x, y)
{
  stopifnot(ncol(x) == ncol(y))
  if (nrow(x) == 0)
    return(y)
  if (nrow(y) == 0)
    return(x)
  y$layout$t <- y$layout$t + nrow(x)
  y$layout$b <- y$layout$b + nrow(x)
  x$layout <- rbind(x$layout, y$layout)

  x$heights <- gtable:::insert.unit(x$heights, y$heights)
  x$rownames <- c(x$rownames, y$rownames)

  x$widths <- grid::unit.pmax(x$widths, y$widths)
  x$grobs <- append(x$grobs, y$grobs)
  x
}

#' Align a number of _like_ ggplots in a horizontal arrangement
#'
#' @param ...
#'     two or more ggplot objects
#' @return
#'     a gtable object, to be rendered using grid.draw()
#' @author Shiv Venkatasubrahmanyam
#' @import ggplot2
#' @export
align.plots.horiz <- function (...)
{
  plots <- list(...)

  if (length(plots) == 1) return(ggplotGrob(plots[[1]]))
  if (length(plots) == 2) return(cbind_gtable_max(ggplotGrob(plots[[1]]),
        ggplotGrob(plots[[2]])))
  return(cbind_gtable_max(do.call(align.plots.horiz, head(plots, -1)),
      ggplotGrob(tail(plots, 1)[[1]])))
}

#' Align a number of _like_ ggplots in a vertical arrangement
#'
#' @param ...
#'     two or more ggplot objects
#' @return
#'     a gtable object, to be rendered using grid.draw()
#' @author Shiv Venkatasubrahmanyam
#' @import ggplot2
#' @export
align.plots.vert <- function (...)
{
  plots <- list(...)

  if (length(plots) == 1) return(ggplotGrob(plots[[1]]))
  if (length(plots) == 2) return(rbind_gtable_max(ggplotGrob(plots[[1]]),
        ggplotGrob(plots[[2]])))
  return(rbind_gtable_max(do.call(align.plots.vert, head(plots, -1)),
      ggplotGrob(tail(plots, 1)[[1]])))
}

# See also:
#   http://stackoverflow.com/a/13657460 (grid.arrange approach)
#   https://raw.github.com/jbryer/multilevelPSA/master/R/align.R (update of ggExtra approach)
