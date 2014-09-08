#' Plot LOD Scores From Merlin
#'
#' @param dat
#'      dataframe with columns Chr, Pos, LOD and Replicate
#' @param chr
#'      character vector of Chr levels to include in the plot. The default
#'      value, "all", plots all data in dat
#' @return
#'      ggplot
#' @export
plotMerlinLod <- function(dat, chr = "all") {
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
           geom_line(alpha = 0.1) +
           labs(x = "Position in cM", y = "HLOD") +
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
                             color = "red", alpha = 0.3)
    return(plt)
}

plotNonPAllChr <- function(dat) {
    dat <- subset(dat,
                  subset = !is.na(Chr) &
                           as.character(Analysis) == nonPAnalysisName,
                  select = c(Chr, Pos, ExLOD, Replicate))
    levels(dat$Chr) <- paste("chr", levels(dat$Chr))
    plt <- ggplot(data = dat,
                  mapping = aes(x = Pos, y = ExLOD, group = Replicate)) +
           geom_line(alpha = 0.1) +
           facet_grid(~ Chr, scales = "free_x", space = "free_x") +
           scale_x_continuous(breaks = c(0, 100, 200)) +
           labs(x = "Position in cM", y = "LOD") +
           theme(panel.grid = element_blank())
    plt <- plt + geom_abline(intercept = 3, slope = 0,
                             color = "red", alpha = 0.3)
    return(plt)
}
plotNonPOneChr <- function(dat, chr) {
    dat <- subset(dat,
                  subset = as.character(Chr) == chr &
                           as.character(Analysis) == nonPAnalysisName,
                  select = c(Pos, ExLOD, Replicate))
    plt <- ggplot(data = dat,
                  mapping = aes(x = Pos, y = ExLOD, group = Replicate)) +
           geom_line(alpha = 0.1) +
           scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250)) +
           labs(x = "Position in cM", y = "LOD") +
           ggtitle(sprintf("chr %s", chr)) +
           theme(panel.grid = element_blank())
    plt <- plt + geom_abline(intercept = 3, slope = 0,
                             color = "red", alpha = 0.3)
    return(plt)
}
plotParaAllChr <- function(dat) {
    dat <- subset(dat,
                  subset = as.character(Model) == paraModelName,
                  select = c(Chr, Pos, HLOD, Replicate))
    levels(dat$Chr) <- paste("chr", levels(dat$Chr))
    plt <- ggplot(data = dat,
                  mapping = aes(x = Pos, y = HLOD, group = Replicate)) +
           geom_line(alpha = 0.1) +
           facet_grid(~ Chr, scales = "free_x", space = "free_x") +
           scale_x_continuous(breaks = c(0, 100, 200)) +
           labs(x = "Position in cM", y = "HLOD") +
           theme(panel.grid = element_blank())
    plt <- plt + geom_abline(intercept = 3, slope = 0,
                             color = "red", alpha = 0.3)
    return(plt)
}
plotParaOneChr <- function(dat, chr) {
    dat <- subset(dat,
                  subset = as.character(Chr) == chr &
                           as.character(Model) == paraModelName,
                  select = c(Pos, HLOD, Replicate))
    plt <- ggplot(data = dat,
                  mapping = aes(x = Pos, y = HLOD, group = Replicate)) +
           geom_line(alpha = 0.1) +
           scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250)) +
           labs(x = "Position in cM", y = "HLOD") +
           ggtitle(sprintf("chr %s", chr)) +
           theme(panel.grid = element_blank())
    plt <- plt + geom_abline(intercept = 3, slope = 0,
                             color = "red", alpha = 0.3)
    return(plt)
}
