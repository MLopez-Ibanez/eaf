##' Compute Vorob'ev threshold, expectation and deviation. Also, displaying the symmetric deviation function is possible. 
##' (The symmetric deviation function is the probability for a given target in the objective space to belong to the symmetric
##'  difference between the Vorob'ev expectation and a realization of the (random) attained set.)
##' @title Vorob'ev computations
##' @param x Either a matrix of data values, or a data frame, or a list of data frames of exactly three columns.
##' The third column is assumed to give the run identifier, from 1 to the number of runs.
##' @param reference Reference point as a vector of numerical values.
##' @note Are x's supposed to be sets of Pareto optimal points?
##' @export
##' @return \code{vorobT} returns a list with elements \code{threshold} and \code{VE}
##' @rdname Vorob
##' @author Mickael Binois
##' @examples
##' data(gcp2x2)
##' tabucol <- subset(gcp2x2, alg != "TSinN1")
##' tabucol$alg <- tabucol$alg[drop=TRUE]
##' data_t <- as.matrix(tabucol[tabucol$inst=="DSJC500.5", c("time", "best", "run")])
##' res <- vorobT(data_t, reference = c(2, 200))
##' 
##' # Display Vorob'ev expectation
##' eafplot(x = data_t[,1:2], sets = data_t[,3], percentiles = c(0,25,50,75,100, res$threshold))
##'   
vorobT <- function(x, reference){
  
  x <- check.eaf.data(x)
  
  # First step: compute average hypervolume over conditional Pareto fronts
  avg_hyp <- 0
  nruns <- max(x[,3])
  
  # FIXME: could be performed directly on the C side? 
  for(i in 1:max(x[,3])){
    avg_hyp <- avg_hyp + hypervolume(data = x[which(x[,3] == i), 1:2], reference = reference)
  }
  avg_hyp <- avg_hyp/nruns
  
  prev_hyp <- diff <- Inf # hypervolume of quantile at previous step
  a <- 0
  b <- 100
  while(diff != 0){
    eaf_res <- eafs(x[,1:2], x[,3], percentiles = (a+b)/2)[,1:2]
    tmp <- hypervolume(eaf_res, reference = reference)
    if(tmp > avg_hyp) a <- (a+b)/2 else b <- (a+b)/2
    diff <- prev_hyp - tmp
    prev_hyp <- tmp
  }
  
  return(list(threshold = a, VE = eaf_res))
} 

##' @export
##' @rdname Vorob  
##' @return \code{vorobdev} returns the Vorob'ev deviation.
##' @examples
##' 
##' # Now print Vorob'ev deviation
##' VD <- vorobDev(data_t, res$VE, reference = c(2, 200))
##' print(VD)
##' 
vorobDev <- function(x, VE, reference){
  if(is.null(VE)) VE <- vorobT(x, reference)$VE
  
  nruns <- max(x[,3])
  VD <- 0
  
  # hypervolume of the symmetric difference between A and B: 2*H(AUB) - H(A) - H(B)
  for(i in 1:nruns){
    H1 <- hypervolume(x[x[,3] == i,1:2], reference = reference)
    H2 <- hypervolume(VE, reference = reference)
    H12 <- hypervolume(rbind(x[x[,3] == i,1:2], VE), reference = reference)
    VD = VD + 2*H12 - H1 - H2
  }
  return(VD)
}

##' @param VE,threshold Vorob'ev expectation and threshold, e.g., as returned by \code{\link[eaf]{vorobT}}.
##' @param nlevels number of levels in which is divided the range of the symmetric deviation
##' @param add if \code{FALSE}, a new graph is created
##' @export
##' @rdname Vorob
##' @references
##' M. Binois, D. Ginsbourger and O. Roustant (2015), Quantifying Uncertainty on Pareto Fronts with Gaussian process conditional simulations, European Journal of Operational Research, 243(2), 386-394.\cr\cr 
##' C. Chevalier (2013), Fast uncertainty reduction strategies relying on Gaussian process models, University of Bern, PhD thesis. \cr\cr
##' I. Molchanov (2005), Theory of random sets, Springer.
##' @examples
##'  # Now display symmetric deviation function
##' symDifPlot(data_t, res$VE, res$threshold, add = FALSE, nlevels = 51)
symDifPlot <- function(x, VE, threshold, add = FALSE, nlevels = 21){
  if(!add)  plot(NA, xlim = range(x[,1]), ylim = range(x[,2]),
                 xlab = expression(f[1]), ylab = expression(f[2]))
  lines(VE, col = "cyan", lwd = 2)
  levs <- seq(0, 100, length.out = nlevels)
  tmp <- eafs(x[,1:2], x[,3], percentiles = levs)
  cols <- rev(gray.colors(nlevels))
  
  # Denote p_n the attainment probability, the value of the symmetric difference function
  # is p_n if p_n < alpha (Vorob'ev threshold) and 1 - p_n otherwise.
  for(i in 1:length(levs)){
    if(levs[i] > threshold) lines(tmp[tmp[,3] == levs[i],1:2], col = cols[1-i]) else lines(tmp[tmp[,3] == levs[i],1:2], col = cols[i])
  }
}

