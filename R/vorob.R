##' Compute Vorob'ev threshold, expectation and deviation. Also, displaying the
##' symmetric deviation function is possible.  The symmetric deviation
##' function is the probability for a given target in the objective space to
##' belong to the symmetric difference between the Vorob'ev expectation and a
##' realization of the (random) attained set.
##' 
##' @title Vorob'ev computations
##' @param x Either a matrix of data values, or a data frame, or a list of data
##'   frames of exactly three columns.  The third column gives the set (run,
##'   sample, ...) identifier.
##' @template arg_refpoint
##' @return `vorobT` returns a list with elements `threshold`,
##'   `VE`, and `avg_hyp` (average hypervolume)
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
##'         col = gray(seq(0.8, 0.1, length.out = 6)^0.5), type = "area", 
##'         legend.pos = "bottomleft", extra.points = res$VE, extra.col = "cyan",
##'         extra.legend = "VE", extra.lty = "solid", extra.pch = NA, extra.lwd = 2,
##'         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a, "%"),
##'                           list(a = formatC(res$threshold, digits = 2, format = "f"))))
##' @md
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
    c <- (a + b) / 2
    eaf_res <- eafs(x[,1:nobjs], x[,setcol], percentiles = c)[,1:nobjs]
    tmp <- hypervolume(eaf_res, reference = reference)
    if (tmp > avg_hyp) a <- c else b <- c
    diff <- prev_hyp - tmp
    prev_hyp <- tmp
  }
  
  return(list(threshold = c, VE = eaf_res, avg_hyp = avg_hyp))
} 

##' @export
##' @rdname Vorob  
##' @return `vorobDev` returns the Vorob'ev deviation.
##' @examples
##' 
##' # Now print Vorob'ev deviation
##' VD <- vorobDev(CPFs, res$VE, reference = c(2, 200))
##' print(VD)
##' @md
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
##'   by [vorobT()].
##' @param nlevels number of levels in which is divided the range of the
##'   symmetric deviation.
##' @param ve.col plotting parameters for the Vorob'ev expectation.
##' @param xlim,ylim,main Graphical parameters, see
##'   [`plot.default()`][graphics::plot.default()].
##' @param legend.pos the position of the legend, see
##'   [`legend()`][graphics::legend()]. A value of `"none"` hides the legend.
##' @param col.fun function that creates a vector of `n` colors, see
##'   [`heat.colors()`][grDevices::heat.colors()].
##' @examples
##' # Now display the symmetric deviation function.
##' symDifPlot(CPFs, res$VE, res$threshold, nlevels = 11)
##' # Levels are adjusted automatically if too large.
##' symDifPlot(CPFs, res$VE, res$threshold, nlevels = 200, legend.pos = "none")
##' 
##' # Use a different palette.
##' symDifPlot(CPFs, res$VE, res$threshold, nlevels = 11, col.fun = heat.colors)
##' @md
##' @export
# FIXME: Implement "add=TRUE" option that just plots the lines,points or
# surfaces and does not create the plot nor the legend (but returns the info
# needed to create a legend), so that one can use the function to add stuff to
# another plot.
symDifPlot <- function(x, VE, threshold, nlevels = 11,
                       ve.col = "blue", xlim = NULL, ylim = NULL,
                       legend.pos = "topright", main = "Symmetric deviation function",
                       col.fun = function(n) gray(seq(0, 0.9, length.out = n)^2))
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
  nlevels <- min(length(unique.default(x[, 3])) - 1, nlevels)
  
  threshold <- round(threshold, 4)
  seq.levs <- round(seq(0, 100, length.out = nlevels), 4)
  levs <- sort.int(unique.default(c(threshold, seq.levs)))
  attsurfs <- compute.eaf.as.list(x, percentiles = levs)
    
  # Denote p_n the attainment probability, the value of the symmetric
  # difference function is p_n if p_n < alpha (Vorob'ev threshold) and 1 - p_n
  # otherwise. Therefore, there is a sharp transition at alpha.  For example,
  # for threshold = 44.5 and 5 levels, we color the following intervals:
  #
  # [0, 25) [25, 44.9) [44.9, 50) [50, 75) [75, 100]
  #
  # with the following colors:
  #
  # [0, 25) [25, 50) [50, 75) [25, 50) [0, 25) 
  max.interval <- max(which(seq.levs < max(100 - threshold, threshold)))
  colscale <- seq.levs[1:max.interval]
  # Reversed so that darker colors are associated to higher values
  names(colscale) <- rev(col.fun(max.interval))
  cols <- c(names(colscale[colscale < threshold]),
            rev(names(colscale[1:max(which(colscale < 100 - threshold))])),
            "#FFFFFF") # To have white after worst case
  names(levs) <- cols
    
  # FIXME: We should take the range from the attsurfs to not make x mandatory.
  xlim <- get.xylim(xlim, maximise[1], data = x[,1])
  ylim <- get.xylim(ylim, maximise[2], data = x[,2])
  extreme <- get.extremes(xlim, ylim, maximise, log = log)

  plot(xlim, ylim, type = "n", xlab = "", ylab = "",
       xlim = xlim, ylim = ylim, log = log, axes = FALSE, las = las,
       main = main,
       panel.first = {
         plot.eaf.full.area(attsurfs, extreme = extreme, maximise = maximise, col = cols)
         # We place the axis after so that we get grid lines.
         plot.eaf.axis (xaxis.side, xlab, las = las, sci.notation = sci.notation)
         plot.eaf.axis (yaxis.side, ylab, las = las, sci.notation = sci.notation,
                        line = 2.2)
         plot.eaf.full.lines(list(VE), extreme, maximise,
                             col = ve.col, lty = 1, lwd = 2)
       })

  # Use first.open to print "(0,X)", because the color for 0 is white.
  intervals <- seq.intervals.labels(seq.levs, first.open = TRUE)
  intervals <- intervals[1:max.interval]
  names(intervals) <- names(colscale)
  #names(intervals) <- names(colscale[1:max.interval])
  if (is.na(pmatch(legend.pos, "none")))
    legend(legend.pos, legend = c("VE", intervals), fill = c(ve.col, names(intervals)),
           bg="white", bty="n", xjust=0, yjust=0, cex=0.9)
  box()
}
