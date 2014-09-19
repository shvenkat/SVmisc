ggLiden <- function(haploShare, hsValue, ped, phenotype, ...) {
  # TODO: infer phenotype, support no pedigree
  plotDat <- lidenPlotData(haploShare, phenotype)
  plts <- lidenPlot(plotDat, hsValue, ped, ...)
  return(invisible(plts))
}

#' Plot Haplotype Sharing In The Context Of A Pedigree
#'
#' This function plots chromosome schematics depicting the results of LIden for
#' a single chromosome, optionally aligned to the corresponding pedigree. This
#' allows one to visualize haplotype sharing in the context of pedigree
#' relationships. In addition, a region of interest (say a linkage region) on
#' the chromosome can be highlighted.
#'
#' Plots are drawn before \code{lidenPlot} returns. The caller is responsible
#' for initializing an appropriate graphics device prior to invoking this
#' function (see examples below).
#'
#' @param plotDat
#'      data frame with columns Position, Person, Haplotype and Phenotype, such
#'      as returned by \code{\link{lidenPlotData}}. Each row corresponds to an
#'      allele at a certain chromosomal position and in a particular person
#'      having the specified phenotype that is shared with the founder.
#' @param hsValue
#'      integer value 1 or 2, selecting the corresponding founder haplotype.
#'      Sharing status with respect to select the founder haplotype to plot to plot
#'      haplotype sharing with homolog 1 or 2
#' @param ped
#'      \code{\link[kinship2]{pedigree}} object corresponding to plotDat. This
#'      argument is required if the pedigree is to be drawn above the haplotype
#'      sharing plot.
#' @param phenoScale
#'      (optional) list used to map plotDat$Phenotype values to a color scale.
#'      If Phenotype is a factor, phenoScale$values must be a named character
#'      vector with a color for each Phenotype level. For a numeric Phenotype,
#'      the 'low' and 'high' elements of phenoScale set the extremes of a
#'      gradient color scale.
#' @param alpha
#'      (optional) numerical value between 0 and 1 specifying opacity of plot
#'      points. Use a small value to avoid overplotting.
#' @param bgColor
#'      (optional) background color
#' @param title
#'      (optional) string to be displayed above the plot
#' @param annotRange
#'      (optional) two-element integer vector indicating the range of
#'      plotDat$Position to be highlighted.
#' @param showLegend
#'      (optional) boolean value to control appearance of plot legends
#' @param ggped
#'      (optional) ggplot object to render the pedigree, as returned by
#'      \code{\link[kinshipExtra]{ggpedigree}}. Use this if you are familiar
#'      with ggplot and need complete control over the appearance of the
#'      pedigree. Plot alignment is not guaranteed.
#' @param plot
#'      a boolean value; if TRUE, plots are drawn on the current graphics
#'      device; if FALSE, plots are returned in a list
#' @return
#'      list of plot objects, for advanced use cases. Most users can ignore the
#'      return value using \code{invisible(lidenPlot(...))} and use this
#'      function to directly render plots, as long as the appropriate graphics
#'      device has already been opened.
#' @examples
#' \dontrun{
#'      library(kinshipExtra)
#'      library(gridExtra)
#'      svg("haploshare.svg", width = 10, height = 15)
#'      invisible(lidenPlot(...))
#'      dev.off()
#' }
#' @import kinshipExtra gridExtra
#' @export
lidenPlot <- function(plotDat, hsValue, ped,
                      phenoScale, alpha = 0.05, bgColor = "gray70",
                      title = "", annotRange, showLegend = TRUE,
                      ggped, plot = TRUE) {
  # TODO: support plot argument

  # CHECK ARGUMENTS ---------------------------------------------------------

  if(missing(hsValue) || ! hsValue %in% 1:2)
    stop("hsValue must be the integer 1 or 2")

  if(missing(ped)) {
    showPed <- FALSE
  } else {
    showPed <- TRUE
    if(!all(levels(plotDat$Person) %in% ped$id)) {
      warning(paste("Person IDs present in the haplotype sharing data",
                    "but not in the pedigree are omitted in the plot"))
    }
    plotDat <- subset(plotDat, subset = as.character(Person) %in% ped$id)
    if(length(unique(as.character(plotDat$Person))) < 2) {
      stop(paste("Identifiers in the haplotype sharing data do not match",
           "those in the pedigree"))
    }

    # Replace Person IDs with horizontal plot positions
    pedAlign <- alignped(ped, method = "1d")
    pedHpos <- hposped(ped, pedAlign)
    plotDat$Person <- pedHpos[levels(plotDat$Person)][plotDat$Person]
    if(missing(ggped)) {
      ggped <- ggpedigree(ped, alignped(ped, method = "1d"),
                          bgcolor = "gray70")
    }
  }

  if(missing(phenoScale)) {
    if(is.factor(plotDat$Phenotype)) {
      phenoScale <- list(values = switch(as.character(nlevels(plotDat$Phenotype)),
                                         "1" = "white",
                                         "2" = c("white", "gray25"),
                                         hue_pal()(nlevels(plotDat$Phenotype))))
      names(phenoScale$values) <- levels(plotDat$Phenotype)
    } else {
      phenoScale <- list(low = "white", high = "black")
    }
  }

  if(!missing(annotRange)) {
    showRange <- TRUE
    if(any(annotRange < min(plotDat$Position)) ||
       any(annotRange > max(plotDat$Position))) {
      stop("annotRange extends beyond the range of plotDat$Position")
    }
  } else {
    showRange <- FALSE
  }

  # BUILD HAPLOTYPE SHARING PLOT --------------------------------------------

  plotDat <- plotDat[plotDat$Haplotype == hsValue | plotDat$Haplotype == 3, ]
  plt <- ggplot(data = plotDat,
                mapping = aes(x = Person, y = Position, color = Phenotype)) +
         geom_point(alpha = alpha) +
         labs(x = "Participant ID", y = "Chromosomal Position (Mb)") +
         scale_y_continuous(trans = reverse_trans(),
                            labels = function(x) x/1000000)
  if(showPed) {
    pedXRange <- ggplot_build(ggped)$panel$ranges[[1]]$x.range
    plt <- plt + scale_x_continuous(breaks = pedHpos, labels = names(pedHpos),
                                    limits = pedXRange, expand = c(0,0),
                                    oob = rescale_none)
  }
  if(is.factor(plotDat$Phenotype)) {
    plt <- plt + scale_color_manual(values = phenoScale$values)
  } else {
    plt <- plt + scale_color_gradient(low  = phenoScale$low,
                                      high = phenoScale$high)
  }
  if(showRange) {
    plt <- plt +
           annotate(geom = "rect",
                    ymin = annotRange[1], ymax = annotRange[2],
                    xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.2)
  }

  # THEME, ALIGN AND DRAW PLOTS ----------------------------------------------------

  plt <- plt +
         theme(panel.background = element_rect(fill = bgColor),
               panel.grid = element_blank(),
               plot.margin = unit(c(0, 1, 0.5, 0.5), "lines"))
  ggped <- ggped +
           theme(plot.margin = unit(c(1, 1, 0, 0.5), "lines"))
  if(showLegend) {
    plt <- plt +
           guides(color = guide_legend(title = "",
                                       override.aes = list(alpha = 1,
                                                           shape = 15,
                                                           size = 4))) +
           theme(legend.key = element_rect(fill = bgColor))
  } else {
    plt <- plt + guides(color = FALSE)
    ggped <- ggped + guides(fill = FALSE, color = FALSE, alpha = FALSE)
  }

  l <- ggLegend(plt)
  g1 <- ggplotGrob(plt + theme(legend.position = "none"))
  g2 <- ggplotGrob(ggped + theme(legend.position = "none"))
  g1$widths <- g2$widths <- unit.pmax(g1$widths, g2$widths)
  grid.arrange(g2, l, g1,
               ncol = 2, nrow = 2,
               widths = c(1, 0.2), heights = c(0.67, 1),
               main = title)

  # return(list(plt, ggped))
  return(invisible(NULL))
}

#' Construct data frame for a ggplot of pedigree haplotype sharing
#'
#' @param haploShare
#'      haplotype sharing matrix, as returned by
#'      \code{\link{readLidenHaploShare}}
#' @param phenotype
#'      named factor, character or numeric vector, giving the phenotype of
#'      each person in the pedigree
#' @return
#'      data frame with columns Position, Person, Haplotype and Phenotype. Each
#'      row corresponds to an allele at a certain chromosomal position and in a
#'      particular person having the specified phenotype that is shared with
#'      one of the two founder haplotypes.
#' @import kinshipExtra
#' @export
lidenPlotData <- function(haploShare, phenotype) {

  position <- as.integer(rownames(haploShare))
  if(!all(is.finite(position)))
    stop("Error: chromosomal positions are not integer-valued")
  dat <- data.frame("Position" = position, haploShare, row.names = NULL,
                    check.names = FALSE, stringsAsFactors = FALSE)
  dat <- melt(data = dat, id.vars = "Position", variable.name = "Person",
              value.name = "Haplotype")
  dat <- dat[dat$Haplotype != 0, ]

  if(is.factor(phenotype) || is.numeric(phenotype)) {
    dat$Phenotype <- phenotype[levels(dat$Person)][dat$Person]
  } else if(is.character(phenotype)) {
    dat$Phenotype <- factor(phenotype[levels(dat$Person)][dat$Person])
  }

  return(dat)
}
