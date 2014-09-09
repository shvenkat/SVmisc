#' Plot LOD Scores From Merlin
#'
#' @param dat
#'      dataframe with columns Chr, Pos, LOD and Replicate
#' @param chr
#'      character vector of Chr levels to include in the plot. The default
#'      value, "all", plots all data in dat
#' @param hilight
#'      optional dataframe with columns Chr, LeftPos and RightPos specifying
#'      regions to be highlighted
#' @return
#'      ggplot
#' @export
plotMerlinLod <- function(dat, chr = "all", hilight = NULL) {
    if(!is.factor(dat$Chr)) {
        dat$Chr <- factor(dat$Chr)
    }
    if(chr == "all") {
        chr <- levels(dat$Chr)
    }
    dat <- subset(dat, subset = as.character(Chr) %in% chr)
    levels(dat$Chr) <- paste("chr", levels(dat$Chr))
    chr <- paste("chr", chr)
    plt <- ggplot(data = dat,
                  mapping = aes(x = Pos, y = LOD, group = Replicate)) +
           geom_line(alpha = 0.2) +
           labs(x = "Chromosomal Position (cM)", y = "LOD") +
           theme(panel.grid = element_blank())
    if(length(chr) > 1) {
        plt <- plt +
               facet_grid(~ Chr, scales = "free_x", space = "free_x") +
               scale_x_continuous(breaks = c(0, 100, 200))
    } else {
        plt <- plt +
               ggtitle(chr) +
               scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250))
    }
    plt <- plt + geom_abline(intercept = 3, slope = 0,
                             color = "red", alpha = 0.5, linetype = 2)
    if(!is.null(hilight)) {
        hilight$Chr <- factor(paste("chr", as.character(hilight$Chr)))
        hilight <- subset(hilight, subset = Chr %in% chr)
        plt <- plt +
               geom_rect(data = hilight,
                         mapping = aes(xmin = LeftPos, xmax = RightPos),
                         inherit.aes = FALSE,
                         ymin = -Inf, ymax = Inf,
                         fill = "red", alpha = 0.15,
                         color = "red", size = 0)
    }
    return(plt)
}
