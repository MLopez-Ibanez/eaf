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
##'         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a),
##'                           list(a = formatC(res$threshold, digits = 2))))
##' 
##' # Second style
##' eafplot(CPFs[,1:2], sets = CPFs[,3], percentiles = c(0, 20, 40, 60, 80, 100),
##'         col=gray(seq(0.8, 0.1, length.out = 6)^0.5), type = "area", 
##'         legend.pos = "bottomleft", extra.points = res$VE, extra.col = "cyan", extra.legend = "VE",
##'         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a),
##'                           list(a = formatC(res$threshold, digits = 2))))
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
  if(is.null(VE)) VE <- vorobT(x, reference)$VE
  if(class(x) == "data.frame") x <- as.matrix(x)
  
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

# ##' @param VE,threshold Vorob'ev expectation and threshold, e.g., as returned
# ##'   by \code{\link[eaf]{vorobT}}.
# ##' @param nlevels number of levels in which is divided the range of the
# ##'   symmetric deviation
# ##' @param add if \code{FALSE}, a new graph is created
# ##' @export
# ##' @rdname Vorob
# ##' @references
# ##' 
# ##' M. Binois, D. Ginsbourger and O. Roustant (2015), Quantifying Uncertainty
# ##' on Pareto Fronts with Gaussian process conditional simulations, European
# ##' Journal of Operational Research, 243(2), 386-394.
# ##'
# ##' C. Chevalier (2013), Fast uncertainty reduction strategies relying on
# ##' Gaussian process models, University of Bern, PhD thesis.
# ##'
# ##' I. Molchanov (2005), Theory of random sets, Springer.
# ##'
# ##' @examples
# ##' # Now display symmetric deviation function
# ##' symDifPlot(data_t, res$VE, res$threshold, add = FALSE, nlevels = 21)
# ##' 
# symDifPlot <- function(x, VE, threshold, add = FALSE, nlevels = 21,
#                        ve.col = "red", ve.lwd = 3, ve.lty = "dashed")
# {
#   if(!add)
#     plot(NA, xlim = range(x[,1]), ylim = range(x[,2]),
#          xlab = expression(f[1]), ylab = expression(f[2]))
#   levs <- seq(0, 100, length.out = nlevels)
#   tmp <- eafs(x[,1:2], x[,3], percentiles = levs)
#   cols <- rev(gray.colors(nlevels))
#   
#   # Denote p_n the attainment probability, the value of the symmetric
#   # difference function is p_n if p_n < alpha (Vorob'ev threshold) and 1 - p_n
#   # otherwise.
# 
#   ## FIXME: I think the code in the for-loop may generate cols[0], which is
#   ## invalid. The code below should be correct.
#   #  cols <- ifelse(levs > threshold, rev(cols), cols)
#   # Then the for-loop can use col = cols[i]
#   
#   for(i in 1:length(levs)) {
#     lines(tmp[tmp[,3] == levs[i], 1:2], col = if (levs[i] > threshold) cols[nlevels-i] else cols[i])
#   }
# 
#   lines(VE, col = ve.col, lwd = ve.lwd, lty = ve.lty)
#   
#   legend.txt <- c("VE", sprintf("%.2g %%", levs / 100))
#   legend.pos <- "topright"
#   legend(x = legend.pos, y = NULL,
#          legend = legend.txt, xjust=1, yjust=1, bty="n",
#          lty = c(ve.lty, rep("solid", nlevels)),
#          lwd = c(ve.lwd, rep(1, nlevels)),
#          col = c(ve.col, ifelse(levs > threshold, rev(cols), cols)))
# }
# 
# ##' @param VE,threshold Vorob'ev expectation and threshold, e.g., as returned
# ##'   by \code{\link[eaf]{vorobT}}.
# ##' @param nlevels number of levels in which is divided the range of the
# ##'   symmetric deviation
# ##' @param add if \code{FALSE}, a new graph is created
# ##' @export
# ##' @rdname Vorob
# ##' @examples
# ##' # Now display symmetric deviation function
# ##' symDifPlot2(data_t, res$VE, res$threshold, add = FALSE, nlevels = 21)
# ##'
# symDifPlot2 <- function(x, VE, threshold, add = FALSE, nlevels = 21,
#                        ve.col = "red", ve.lwd = 3, ve.lty = "dashed")
# {
#   levs <- seq(0, 100, length.out = nlevels)
# 
#   attsurfs <- compute.eaf.as.list(x, percentiles = levs)
# 
#   cols <- gray.colors(nlevels)
#   # Denote p_n the attainment probability, the value of the symmetric
#   # difference function is p_n if p_n < alpha (Vorob'ev threshold) and 1 - p_n
#   # otherwise.
#   cols <- ifelse(levs > threshold, cols, rev(cols))
#   eafplot.default(x, attsurfs = c(attsurfs,list(VE=VE)),
#                   percentiles = levs, col = c(cols,ve.col),
#                   lty = c(rep("solid", nlevels), ve.lty))
# }

##' @param VE,threshold Vorob'ev expectation and threshold, e.g., as returned
##'   by \code{\link[eaf]{vorobT}}.
##' @param nlevels number of levels in which is divided the range of the
##'   symmetric deviation
##' @param add if \code{FALSE}, a new graph is created
##' @param ve.col,ve.lwd,ve.lty plotting parameters for the Vorob'ev expectation
##' @export
##' @rdname Vorob
##' @examples
##' # Now display symmetric deviation function
##' symDifPlot(CPFs, res$VE, res$threshold, add = FALSE, nlevels = 8)
##'
symDifPlot <- function(x, VE, threshold, add = FALSE, nlevels = 8,
                        ve.col = "red", ve.lwd = 3, ve.lty = "dashed")
{
  levs <- sort(c(threshold, seq(0, 100, length.out = nlevels)))
  
  attsurfs <- compute.eaf.as.list(x, percentiles = levs)
  
  cols <- gray(seq(0, 0.9, length.out = nlevels+1)^2)
  # Denote p_n the attainment probability, the value of the symmetric
  # difference function is p_n if p_n < alpha (Vorob'ev threshold) and 1 - p_n
  # otherwise.
  cols <- ifelse(levs >= threshold, cols, rev(cols))
  cols[nlevels + 1] <- "#FFFFFF" # To have white after worse case
  
  eafplot.default(x, attsurfs = attsurfs,
                  legend.txt = c(ifelse(levs <= threshold, levs, rev(levs))),
                  percentiles = levs, col = cols,
                  extra.points = VE, extra.col = ve.col, extra.lty = 1,
                  lty = "solid", type = "area",
                  main = "Symmetric deviation function")
}
