#' Plot EcMap allele-sharing simulation results
#' 
#' Generates a ggplot "histogram" of IBD region size in cM and the proportion 
#' of simulations in which regions of each size were observed. Causal variant-
#' containing regions and those shared by chance are distinguished by line type.
#' Smoothed lines are plotted due to the noise in the data. Simulation 
#' parameters are indicated (as a set not individually).
#' 
#' @param files
#'     character vector of paths to Ecmap analysis.txt files
#' @param nSimulations
#'     integer vector of same length as files, giving the corresponding number 
#'     of simulations
#' @param simParamValues
#'     character vector of same length as files, with strings summarizing the 
#'     parameters (e.g. genetic model, penetrance, phenocopy, number of 
#'     simulations) of the corresponding simulation e.g. c("1000", "10000")
#' @param SimParamName
#'     string giving the pretty label for simulation parameters (e.g. "Number 
#'     of simulations")
#' @return
#'     ggplot object
#' @author Shiv Venkatasubrahmanyam
#' @import ggplot2 scales
#' @export
getEcmapPlot <- function(files, nSimulations, simParamValues, 
  simParamName = "Simulation parameters") {
  
  data <- getDataForEcmapGgplot(files, nSimulations, simParamValues, 
    simParamName)
  p <- ggplot(data, aes_string(x = "Size", y = "Frequency", 
        colour = sprintf("`%s`", simParamName), linetype = "Category")) + 
    scale_x_continuous(name = "Size (cM)") +
    scale_linetype_manual(values = c(Causal = "solid", Random = "44")) +
    geom_line(size = 0.3, alpha = 0.5) +
    geom_smooth(method = "loess", span = 0.3, size = 1.0, se = FALSE)
  p
}

#' Generate data for EcMap plot
#' 
#' @inheritParams getEcmapPlot
#' @return 
#'     data.frame with columns Size, Frequency, Category and simParamName
#' @author Shiv Venkatasubrahmanyam
#' @import reshape2
#' @keywords internal
getDataForEcmapGgplot <- function(files, nSimulations, simParamValues, 
  simParamName) {
  
  # Check arguments
  if (length(files) < 1)
    stop("files must have length 1 or greater")
  if (length(files) != length(nSimulations))
    stop(sprintf("files and nSimulations differ in length: %i, %i", 
        length(files), length(nSimulations)))
  if (length(files) != length(simParamValues))
    stop(sprintf("files and simParamValues differ in length: %i, %i", 
        length(files), length(simParamValues)))
  if (length(simParamName) != 1)
    stop("simParamName must be a single string")
  
  # Generate result
  data <- mapply(FUN = function(file, nSim, simParam) {
      x <- readEcmapAnalysisFile(file)
      x <- x/nSim
      x <- cbind(as.data.frame(x), Size = as.integer(rownames(x)))
      x <- melt(x, id.vars = "Size", variable.name = "Category", 
        value.name = "Frequency")
      x <- cbind(x, 
#        Simulations = as.character(nSim), 
        simParam = simParam)
      labels <- colnames(x)
      labels[labels == "simParam"] <- simParamName
      colnames(x) <- labels
      x
    }, files, nSimulations, simParamValues, SIMPLIFY = FALSE)
  data <- do.call(rbind, data)
  data <- within(data, {
      Size = as.integer(Size)
      Category = factor(Category)
      Frequency = as.numeric(Frequency)
#      Simulations <- factor(Simulations)
    })
  data[[simParamName]] <- factor(data[[simParamName]])
  data
}
