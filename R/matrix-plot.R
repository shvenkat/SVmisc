#' @include recycle.R
NULL

#' Line plot with each series drawn from the columns of a matrix.
#' 
#' @param x vector
#' @param y matrix, with as many rows as elements in x
#' @param label labels used in the legend, vector with as many elements as 
#'            columns in y
#' @param type plot type [see lines()], single character
#' @param lty line type [see par()], vector
#' @param lwd line width [see par()], vector
#' @param col line/symbol color, vector
#' @param pch plotting symbol, vector
#' @param xLegend x coordinate to position the legend
#' @param yLegend y coordinate to position the legend
#' @param cexLegend text size in legend
#' @param ... additional parameters passed to plot (e.g. xlab, ylab, main)
#' @return null
#' @author Shiv Venkatasubrahmanyam
#' @export
multiLinePlot <- function(x, y, label = colnames(y), type = "b", lty = 1, 
        lwd = 1, col = "black", pch = 19, xLegend = range(x)[1], 
        yLegend = range(y)[2], cexLegend = 1, ...) {
    
    # bare plot 
    plot(range(x), range(y), type="n", ...)
    
    lty <- recycle(lty, ncol(y))
    lwd <- recycle(lwd, ncol(y))
    col <- recycle(col, ncol(y))
    pch <- recycle(pch, ncol(y))
    
    # add one line per series (column of y) 
    for (i in 1:ncol(y)) { 
        lines(x, y[, i], type = type, lty = lty[i], lwd = lwd[i], 
                col = col[i], pch = pch[i]) 
    }

    # legend 
    legend(xLegend, yLegend, label, xjust = 0.5, yjust = 0.5, inset = 0.05, 
            cex = cexLegend, lty = lty, lwd = lwd, col = col, pch = pch, 
            title = "Legend")
        
    NULL
}