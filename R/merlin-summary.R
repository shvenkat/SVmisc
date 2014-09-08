#' Summarize Linkage Regions By Merlin LOD Score
#'
#' @param dat
#'      dataframe with columns Chr, Pos, Label, LOD, Replicate
#' @param lodCutoff
#'      threshold LOD score; loci with higher scores are considered linked
#' @param confInt
#'      pseudo confidence interval. For instance, a value of 0.9 returns
#'      linkage regions whose boundaries exclude loci that are not linked (i.e.
#'      LOD <= lodCutoff) in at least 90\% of replicates. In other words, the
#'      linkage region boundaries include only loci found to be linked in over
#'      10\% of replicates. In addition, the maximum LOD score within each
#'      linkage region is the mean of the maximum LOD scores across replicates,
#'      trimmed to the median confInt values. In the above example, the maximum
#'      LOD score is the mean of the middle 90\% of per-replicate maximum LOD
#'      scores i.e. trimming off 5\% of lowest and highest values.
#' @return
#'      dataframe with columns Chr, LeftPos, LeftLabel, RightPos, RightLabel
#'      and MaxLOD
#' @export
summaryMerlinLodPeaks <- function(dat, lodCutoff, confInt) {
    summaryPeaks <- do.call(rbind,
                            by(dat, dat$Chr,
                               function(x)
                                   summaryMerlinLodPeaksPerChr(x,
                                                               lodCutoff,
                                                               confInt),
                               simplify = FALSE))
    rownames(summaryPeaks) <- NULL
    return(summaryPeaks)
}

#' Summarize Linkage Regions By Merlin LOD Score For One Chromosome
#'
#' @inheritParams summaryMerlinLodPeaks
#' @return
#'      dataframe with columns Chr, LeftPos, LeftLabel, RightPos, RightLabel
#'      and MaxLOD
summaryMerlinLodPeaksPerChr <- function(dat, lodCutoff, confInt) {
    peaks <- do.call(rbind, by(dat, dat$Replicate,
                               function(x) merlinLodPeaks(x, lodCutoff),
                               simplify = FALSE))
    n <- sapply(dat$Pos,
                function(x)
                    sum(x >= peaks$LeftPos & x <= peaks$RightPos))
    summaryDat <- data.frame(Chr       = dat$Chr,
                             Pos       = dat$Pos,
                             Label     = dat$Label,
                             LOD       = n / nlevels(dat$Replicate),
                             Replicate = dat$Replicate)
    summaryPeaks <- merlinLodPeaks(summaryDat,
                                   lodCutoff = round(1 - confInt, 3))
    summaryPeaks$MaxLOD <- apply(summaryPeaks, 1,
                                 function(x) {
                                     inPeak <- dat$Pos >= x["LeftPos"] &
                                               dat$Pos <= x["RightPos"]
                                     maxLod <- by(dat$LOD[inPeak],
                                                  dat$Replicate[inPeak],
                                                  max)
                                     mean(maxLod,
                                          trim = round(1 - confInt, 3) / 2)
                                 })
    rownames(summaryPeaks) <- NULL
    return(subset(summaryPeaks, select = -Replicate))
}

#' Find LOD Score Peaks For One Chromosome And Replicate
#'
#' @inheritParams summaryMerlinLodPeaks
#' @return
#'      dataframe with columns Chr, Replicate, LeftPos, LeftLabel, RightPos,
#'      RightLabel and MaxLOD
merlinLodPeaks <- function(dat, lodCutoff) {
    dat <- dat[order(dat$Pos), ]
    len <- nrow(dat)
    inPeak     <- dat$LOD > lodCutoff
    inPeakPrev <- c(FALSE, inPeak[-len])
    inPeakNext <- c(inPeak[-1], FALSE)
    leftEdge   <- inPeak & !inPeakPrev
    rightEdge  <- inPeak & !inPeakNext
    if(sum(leftEdge) != sum(rightEdge))
        stop("Internal error: unequal number of left and right peak edges")
    peaks <- data.frame("Chr"        = dat$Chr[leftEdge],
                        "Replicate"  = dat$Replicate[leftEdge],
                        "LeftPos"    = dat$Pos[leftEdge],
                        "LeftLabel"  = dat$Label[leftEdge],
                        "RightPos"   = dat$Pos[rightEdge],
                        "RightLabel" = dat$Label[rightEdge])
    peaks$MaxLOD <- apply(peaks, 1,
                          function(x) {
                               inPeak <- dat$Pos >= x["LeftPos"] &
                                         dat$Pos <= x["RightPos"]
                               return(max(dat$LOD[inPeak]))
                            })
    rownames(peaks) <- NULL
    return(peaks)
}
