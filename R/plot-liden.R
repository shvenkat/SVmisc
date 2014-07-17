#' Construct data frame for a ggplot of pedigree haplotype sharing
#'
#' This function returns a data frame given a pedigree and a LIden haplotype
#' sharing matrix. The format of the data frame simplifies use of ggplot.
#'
#' @param haploShare
#'      haplotype sharing matrix, as returned by
#'      \code{\link{readLidenHaploShare}}
#' @param ped
#'      pedigree object, as returned by \code{\link[kinship2]{pedigree}}
#' @param pedHpos
#'      named numeric vector, such as returned by
#'      \code{\link[kinshipExtra]{hposped}}, giving the horizontal position of
#'      each person in the pedigree
#' @return
#'      data frame with columns Position, Person, Haplotype and Phenotype Each
#'      row corresponds to an allele at a certain chromosomal position and in a
#'      particular person having the specified phenotype that is shared with
#'      one of the two founder haplotypes.
#' @import kinshipExtra
#' @export
lidenPlotData <- function(haploShare, ped, pedHpos) {

  position <- as.integer(rownames(haploShare))
  if(!all(is.finite(position)))
    stop("Error: chromosomal positions are not integer-valued")
  dat <- data.frame("Position" = position, haploShare, row.names = NULL,
                    check.names = FALSE, stringsAsFactors = FALSE)
  dat <- melt(data = dat, id.vars = "Position", variable.name = "Person",
              value.name = "Haplotype")
  dat <- dat[!is.na(dat$Haplotype), ]

  # Add Phenotype column using pedigree info
  phenotype <- c("unaffected", "affected")[ped$affected + 1]
  names(phenotype) <- ped$id
  dat$Phenotype <- factor(phenotype[levels(dat$Person)][dat$Person],
                          levels = c("unaffected", "affected"))

  # Replace Person IDs with horizontal plot positions
  dat$Person <- pedHpos[levels(dat$Person)][dat$Person]

  return(dat)
}
