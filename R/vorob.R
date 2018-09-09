##' Compute Vorob'ev threshold, expectation and deviation. Also, displaying the
##' symmetric deviation function is possible.  The symmetric deviation
##' function is the probability for a given target in the objective space to
##' belong to the symmetric difference between the Vorob'ev expectation and a
##' realization of the (random) attained set.
##' 
##' @title Vorob'ev computations
##' @param x Either a matrix of data values, or a data frame, or a list of data
##'   frames of exactly three columns.  The third column is assumed to give the
##'   run identifier, from 1 to the number of runs.
##' @param reference Reference point as a vector of numerical values.
##' @note Are x's supposed to be sets of Pareto optimal points?
##' @return \code{vorobT} returns a list with elements \code{threshold} and
##'   \code{VE}
##' @rdname Vorob
##' @author Mickael Binois
##' @examples
##' data(CPFs)
##' res <- vorobT(CPFs, reference = c(2, 200))
##' print(res$threshold)
##' 
##' ## Display Vorob'ev expectation and attainment function
##' # First style
##' eafplot(CPFs[,1:2], sets = CPFs[,3], percentiles = c(0, 25, 50, 75, 100, res$threshold),
##'         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a, "%"),
##'                           list(a = formatC(res$threshold, digits = 2, format = "f"))))
##' 
##' # Second style
##' eafplot(CPFs[,1:2], sets = CPFs[,3], percentiles = c(0, 20, 40, 60, 80, 100),
##'         col=gray(seq(0.8, 0.1, length.out = 6)^0.5), type = "area", 
##'         legend.pos = "bottomleft", extra.points = res$VE, extra.col = "cyan", extra.legend = "VE",
##'         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a, "%"),
##'                           list(a = formatC(res$threshold, digits = 2, format = "f"))))
##' 
##' @export
vorobT <- function(x, reference)
{
  x <- check.eaf.data(x)
  setcol <- ncol(x)
  nobjs <- setcol - 1L

  # First step: compute average hypervolume over conditional Pareto fronts
  avg_hyp <- mean(sapply(split.data.frame(x[,1:nobjs], x[, setcol]),
                         hypervolume, reference = reference))

  prev_hyp <- diff <- Inf # hypervolume of quantile at previous step
  a <- 0
  b <- 100
  while (diff != 0) {
    eaf_res <- eafs(x[,1:nobjs], x[,setcol], percentiles = (a + b) / 2)[,1:nobjs]
    tmp <- hypervolume(eaf_res, reference = reference)
    if (tmp > avg_hyp) a <- (a+b)/2 else b <- (a+b)/2
    diff <- prev_hyp - tmp
    prev_hyp <- tmp
  }
  
  return(list(threshold = a, VE = eaf_res))
} 

##' @export
##' @rdname Vorob  
##' @return \code{vorobDev} returns the Vorob'ev deviation.
##' @examples
##' 
##' # Now print Vorob'ev deviation
##' VD <- vorobDev(CPFs, res$VE, reference = c(2, 200))
##' print(VD)
##' 
vorobDev <- function(x, VE, reference)
{
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.null(VE)) VE <- vorobT(x, reference)$VE
  
  setcol <- ncol(x)
  nobjs <- setcol - 1L

  # Hypervolume of the symmetric difference between A and B:
  # 2 * H(AUB) - H(A) - H(B)
  H2 <- hypervolume(VE, reference = reference)
  x.split <- split.data.frame(x[,1:nobjs], x[,setcol])
  H1 <- mean(sapply(x.split, hypervolume, reference = reference))

  hv.union.VE <- function(y)
    return(hypervolume(rbind(y[, 1:nobjs], VE), reference = reference))
  
  VD <- 2 * sum(sapply(x.split, hv.union.VE))
  nruns <- length(x.split)
  return((VD / nruns) - H1 - H2)
}

##' @rdname Vorob
##' @references
##' 
##' M. Binois, D. Ginsbourger and O. Roustant (2015), Quantifying Uncertainty
##' on Pareto Fronts with Gaussian process conditional simulations, European
##' Journal of Operational Research, 243(2), 386-394.
##'
##' C. Chevalier (2013), Fast uncertainty reduction strategies relying on
##' Gaussian process models, University of Bern, PhD thesis.
##'
##' I. Molchanov (2005), Theory of random sets, Springer.
##'
##' @param VE,threshold Vorob'ev expectation and threshold, e.g., as returned
##'   by \code{\link[eaf]{vorobT}}.
##' @param nlevels number of levels in which is divided the range of the
##'   symmetric deviation.
##' @param ve.col plotting parameters for the Vorob'ev expectation.
##' @param xlim,ylim,main Graphical parameters, see \code{\link{plot.default}}.
##' @param legend.pos the position of the legend, see \code{\link{legend}}.
##' @examples
##' # Now display symmetric deviation function
##' symDifPlot(CPFs, res$VE, res$threshold, nlevels = 11)
##' @export
symDifPlot <- function(x, VE, threshold, nlevels = 11,
                       ve.col = "blue", xlim = NULL, ylim = NULL,
                       legend.pos = "topright", main = "Symmetric deviation function")
{
  # FIXME: These maybe should be parameters of the function in the future.
  maximise <- c(FALSE, FALSE)
  xaxis.side <- "below"
  yaxis.side <- "left"
  log <- ""
  xlab <- colnames(x)[1]
  ylab <- colnames(x)[2]
  las <- par("las")
  sci.notation <- FALSE
  
  threshold <- round(threshold, 4)
  levs <- round(sort(c(threshold, seq(0, 100, length.out = nlevels))), 4)
  
  attsurfs <- compute.eaf.as.list(x, percentiles = levs)
  
  # Right now, the color is p if p < threshold, 1-p otherwise. Given threshold
  # = 44.9219, the color at exactly p==threshold (or slightly larger) should be
  # 100 - 44.9219 = 55.0781, which is a color that we don't plot. That seems
  # strange.
  cols <- colscale <- gray(seq(0.9, 0, length.out = nlevels)^2)
  # Denote p_n the attainment probability, the value of the symmetric
  # difference function is p_n if p_n < alpha (Vorob'ev threshold) and 1 - p_n
  # otherwise.
  iVE <- which(levs == threshold)
  cols[1:(iVE -1)] <- colscale[1:(iVE - 1)]
  cols[iVE:nlevels] <-  colscale[(nlevels + 1 - iVE):1]
  cols[nlevels + 1] <- "#FFFFFF" # To have white after worst case

  # FIXME: We should take the range from the attsurfs to not make x mandatory.
  xlim <- get.xylim(xlim, maximise[1], data = x[,1])
  ylim <- get.xylim(ylim, maximise[2], data = x[,2])
  extreme <- get.extremes(xlim, ylim, maximise, log = log)

  plot(xlim, ylim, type = "n", xlab = "", ylab = "",
       xlim = xlim, ylim = ylim, log = log, axes = FALSE, las = las,
       panel.first = {
         plot.eaf.full.area(attsurfs, extreme, maximise, cols)
         # We place the axis after so that we get grid lines.
         plot.eaf.axis (xaxis.side, xlab, las = las, sci.notation = sci.notation)
         plot.eaf.axis (yaxis.side, ylab, las = las, sci.notation = sci.notation,
                        line = 2.2)
         plot.eaf.full.lines(list(VE), extreme, maximise,
                             col = ve.col, lty = 1, lwd = 2)
       })

  # FIXME: The legend prints [0,10]. However, the color for 0 should be white,
  # like the color of 100. I don't think we are actually plotting 0, but
  # "(0,10)" or even "(1/length(unique(x[,3])), 10)".
  intervals <- seq.intervals.labels(levs)[1:(length(unique(cols)) - 1)]
  legend(legend.pos, legend = c(intervals, "VE"), fill = c(colscale[1:length(intervals)], ve.col),
         bg="white", bty="n", xjust=0, yjust=0, cex=0.9)
  box()
}
