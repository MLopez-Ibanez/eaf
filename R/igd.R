#' Inverted Generational Distance (IGD and IGD+) and Averaged Hausdorff Distance
#'
#' Functions to compute the inverted generational distance (IGD and IGD+) and
#' the averaged Hausdorff distance between nondominated sets of points.
#'
#' @rdname igd
#' @export
#' @template arg_data
#'
#' @template arg_refset
#'
#' @template arg_maximise
#' 
#' @return  (`numeric(1)`) A single numerical value.
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @details
#'
#' The generational distance (GD) of a set \eqn{A} is defined as the distance
#' between each point \eqn{a \in A} and the closest point \eqn{r} in a
#' reference set \eqn{R}, averaged over the size of \eqn{A}. Formally,
#' 
#' \deqn{GD(A,R) = \frac{1}{|A|}\left(\sum_{a\in A}\min_{r\in R} d(a,r)^p\right)^{\frac{1}{p}} }{GD(A,R) = (1/|A|) * ( sum_{a in A} min_{r in R} d(a,r)^p )^(1/p)}
#' where:
#' \deqn{d(a,r) = \sqrt{\sum_{k=1}^M (a_k - r_k)^2} }{d(a,r) = sqrt( sum_{k=1}^M (a_k - r_k)^2)}
#'
#' The inverted generational distance (IGD) is calculated as \eqn{IGD(A,R) = GD(R,A)}
#' with \eqn{p=1}.
#'
#' GD was first proposed by \citet{VelLam1998gp} with \eqn{p=2}. IGD seems to
#' have been mentioned first by \citet{CoeSie2004igd}, however, some people
#' also used the name D-metric for the same concept with \eqn{p=1} and later
#' papers have often used IGD/GD with \eqn{p=1}.
#'
#' IGDX is the application of IGD to decision vectors instead of objective
#' vectors to measure closeness and diversity in decision
#' space \citep{ZhoZhaJin2009igdx}. One can use the functions here directly,
#' just passing the decision vectors as `data`.
#'
#' The modified inverted generational distanced (IGD+) was proposed by
#' \citet{IshMasTanNoj2015igd} to ensure that IGD+ is weakly Pareto compliant,
#' similarly to unary epsilon. It averages over \eqn{|R|} within the exponent
#' \eqn{1/p} and modifies the distance measure as:
#'
#' \deqn{d^+(r,a) = \sqrt{\sum_{k=1}^M (\max\{r_k - a_k, 0\})^2}}{d^+(r,a) = sqrt(sum_{k=1}^M (max {r_k - a_k, 0 })^2)}
#'
#' The average Hausdorff distance (\eqn{\Delta_p}) was proposed by \citet{SchEsqLarCoe2012tec} and
#' it is calculated as:
#'
#' \deqn{\Delta_p(A,R) = \max\{ IGD_p(A,R), IGD_p(R,A) \}}
#'
#' See \citet{BezLopStu2017emo} for a comparison.
#' 
#' @references
#'
#' \insertAllCited{}
#' 
#' @examples
#' extdata_path <- system.file(package="eaf","extdata")
#' path.A1 <- file.path(extdata_path, "ALG_1_dat.xz")
#' path.A2 <- file.path(extdata_path, "ALG_2_dat.xz")
#' A1 <- read_datasets(path.A1)[,1:2]
#' A2 <- read_datasets(path.A2)[,1:2]
#' ref <- filter_dominated(rbind(A1, A2))
#' igd(A1, ref)
#' igd(A2, ref)
#' 
#' @aliases IGDX
#' @md
igd <- function(data, reference, maximise = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data) 
  npoints <- nrow(data)
  if (is.null(reference)) {
    stop("reference cannot be NULL")
  }
  reference <- check_dataset(reference)
  if (ncol(reference) != nobjs)
    stop("data and reference must have the same number of columns")
  reference_size <- nrow(reference)
  
  maximise <- as.logical(rep_len(maximise, nobjs))
    
  return(.Call(igd_C,
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(t(reference)),
               as.integer(reference_size),
               maximise))
}

#' @rdname igd
#' @export
#' @examples
#' # IGD+ (Pareto compliant)
#' igd_plus(A1, ref)
#' igd_plus(A2, ref)
#' 
igd_plus <- function(data, reference, maximise = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data) 
  npoints <- nrow(data)
  if (is.null(reference)) {
    stop("reference cannot be NULL")
  }
  reference <- check_dataset(reference)
  if (ncol(reference) != nobjs)
    stop("data and reference must have the same number of columns")
  reference_size <- nrow(reference)
  
  maximise <- as.logical(rep_len(maximise, nobjs))
    
  return(.Call(igd_plus_C,
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(t(reference)),
               as.integer(reference_size),
               maximise))
}

#' @rdname igd
#' @param p (`integer(1)`) Hausdorff distance parameter (default: `1L`).
#' @export
#' @examples
#' # Average Haussdorff distance
#' avg_hausdorff_dist(A1, ref)
#' avg_hausdorff_dist(A2, ref)
#' @md
avg_hausdorff_dist <- function(data, reference, maximise = FALSE, p = 1L)
{
  data <- check_dataset(data)
  nobjs <- ncol(data) 
  npoints <- nrow(data)
  if (is.null(reference)) {
    stop("reference cannot be NULL")
  }
  reference <- check_dataset(reference)
  if (ncol(reference) != nobjs)
    stop("data and reference must have the same number of columns")
  reference_size <- nrow(reference)
  
  maximise <- as.logical(rep_len(maximise, nobjs))
    
  return(.Call(avg_hausdorff_dist_C,
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(t(reference)),
               as.integer(reference_size),
               maximise,
               as.integer(p)))
}
