#' Hypervolume metric
#'
#' Computes the hypervolume metric with respect to a given reference point
#' assuming minimization of all objectives.
#'
#' @template arg_data
#'
#' @template arg_refpoint
#'
#' @template arg_maximise
#' 
#' @return  A single numerical value.
#'
#' @details The algorithm has \eqn{O(n^{d-2} \log n)} time and linear space
#'   complexity in the worst-case, but experimental results show that the
#'   pruning techniques used may reduce the time complexity even further.
#' 
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @references
#'
#' \insertRef{FonPaqLop06:hypervolume}{eaf}
#'
#' \insertRef{BeuFonLopPaqVah09:tec}{eaf}
#'
#' @examples
#'
#' data(SPEA2minstoptimeRichmond)
#' # The second objective must be maximized
#' # We calculate the hypervolume of the union of all sets.
#' hypervolume(SPEA2minstoptimeRichmond[, 1:2], reference = c(250, 0),
#'             maximise = c(FALSE, TRUE))
#'
#' @export
hypervolume <- function(data, reference, maximise = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data) 
  npoints <- nrow(data)
  if (is.null(reference)) stop("reference cannot be NULL")
  if (length(reference) == 1) reference <- rep_len(reference, nobjs)

  if (any(maximise)) {
    if (length(maximise) == 1) {
      data <- -data
      reference <- -reference
    } else if (length(maximise) != nobjs) {
      stop("length of maximise must be either 1 or ncol(data)")
    }
    data[,maximise] <- -data[,maximise]
    reference[maximise] <- -reference[maximise]
  }
  return(.Call(hypervolume_C,
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(reference)))
}

#' Hypervolume contribution of a set of points
#'
#' Computes the hypervolume contribution of each point given a set of points
#' with respect to a given reference point assuming minimization of all
#' objectives.  Dominated points have zero contribution. Duplicated points have
#' zero contribution even if not dominated, because removing one of them does
#' not change the hypervolume dominated by the remaining set.
#'
#' @template arg_data
#'
#' @template arg_refpoint
#'
#' @template arg_maximise
#' 
#' @return ([numeric]) A numerical vector
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#'@seealso \code{\link{hypervolume}}
#'
#' @references
#'
#' \insertRef{FonPaqLop06:hypervolume}{eaf}
#'
#' \insertRef{BeuFonLopPaqVah09:tec}{eaf}
#'
#' @examples
#'
#' data(SPEA2minstoptimeRichmond)
#' # The second objective must be maximized
#' # We calculate the hypervolume contribution of each point of the union of all sets.
#' hv_contributions(SPEA2minstoptimeRichmond[, 1:2], reference = c(250, 0),
#'             maximise = c(FALSE, TRUE))
#'
#' # Duplicated points show zero contribution above, even if not
#' # dominated. However, filter_dominated removes all duplicates except
#' # one. Hence, there are more points below with nonzero contribution.
#' hv_contributions(filter_dominated(SPEA2minstoptimeRichmond[, 1:2], maximise = c(FALSE, TRUE)),
#'                  reference = c(250, 0), maximise = c(FALSE, TRUE))
#' 
#' @export
hv_contributions <- function(data, reference, maximise = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data) 
  npoints <- nrow(data)
  if (is.null(reference)) stop("reference cannot be NULL")
  if (length(reference) == 1) reference <- rep_len(reference, nobjs)
  
  if (any(maximise)) {
    if (length(maximise) == 1) {
      data <- -data
      reference <- -reference
    } else if (length(maximise) != nobjs) {
      stop("length of maximise must be either 1 or ncol(data)")
    }
    data[,maximise] <- -data[,maximise]
    reference[maximise] <- -reference[maximise]
  }
  return(.Call(hv_contributions_C,
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(reference)
               ))
}

#' Epsilon metric
#'
#' Computes the epsilon metric, either additive or multiplicative.
#'
#' @template arg_data
#'
#' @template arg_refset
#'
#' @template arg_maximise
#' 
#' @return  A single numerical value.
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @details
#'
#' Given objective vectors \eqn{a} and \eqn{b}, \eqn{epsilon(a,b)} is computed
#' in the case of minimization as \eqn{a/b} for the multiplicative variant
#' (respectively, \eqn{a - b} for the additive variant), whereas in the case of
#' maximization it is computed as \eqn{b/a} for the multiplicative variant
#' (respectively, \eqn{b - a} for the additive variant). This allows computing
#' a single value for problems where some objectives are to be maximized while
#' others are to be minimized. Moreover, a lower value corresponds to a better
#' approximation set, independently of the type of problem (minimization,
#' maximization or mixed). However, the meaning of the value is different for
#' each objective type. For example, imagine that \eqn{f1} is to be minimized
#' and \eqn{f2} is to be maximized, and the multiplicative epsilon computed
#' here for \eqn{epsilon(A,B) = 3}. This means that \eqn{A} needs to be
#' multiplied by 1/3 for all \eqn{f1} values and by 3 for all \eqn{f2} values
#' in order to weakly dominate \eqn{B}. This also means that the computation of
#' the multiplicative version for negative values doesn't make sense.
#'
#' @references
#'
#' \insertRef{ZitThiLauFon2003:tec}{eaf}
#' 
#' @examples
#' extdata_path <- system.file(package="eaf","extdata")
#' path.A1 <- file.path(extdata_path, "ALG_1_dat.xz")
#' path.A2 <- file.path(extdata_path, "ALG_2_dat.xz")
#' A1 <- read_datasets(path.A1)[,1:2]
#' A2 <- read_datasets(path.A2)[,1:2]
#' ref <- filter_dominated(rbind(A1, A2))
#' epsilon_additive(A1, ref)
#' epsilon_additive(A2, ref)
#' 
#' @rdname epsilon
#' @md
#' @export
epsilon_additive <- function(data, reference, maximise = FALSE)
  epsilon_common(data = data, reference = reference, maximise = maximise,
                 mul = FALSE)

#' @rdname epsilon
#' @export
#' @examples
#' # Multiplicative version of epsilon metric
#' ref <- filter_dominated(rbind(A1, A2))
#' epsilon_mult(A1, ref)
#' epsilon_mult(A2, ref)
#' 
epsilon_mult <- function(data, reference, maximise = FALSE)
  epsilon_common(data = data, reference = reference, maximise = maximise,
                 mul = TRUE)

epsilon_common <- function(data, reference, maximise, mul)
{
  data <- check_dataset(data)
  nobjs <- ncol(data) 
  npoints <- nrow(data)
  if (is.null(reference)) stop("reference cannot be NULL")
  
  reference <- check_dataset(reference)
  if (ncol(reference) != nobjs)
    stop("data and reference must have the same number of columns")
  reference_size <- nrow(reference)
  
  maximise <- as.logical(rep_len(maximise, nobjs))

  if (mul)
    return(.Call(epsilon_mul_C,
                 as.double(t(data)),
                 as.integer(nobjs),
                 as.integer(npoints),
                 as.double(t(reference)),
                 as.integer(reference_size),
                 maximise))
  else
    return(.Call(epsilon_add_C,
                 as.double(t(data)),
                 as.integer(nobjs),
                 as.integer(npoints),
                 as.double(t(reference)),
                 as.integer(reference_size),
                 maximise))
}
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

#' Normalise points
#'
#' Normalise points per coordinate to a range, e.g., \code{c(1,2)}, where the
#' minimum value will correspond to 1 and the maximum to 2. If bounds are
#' given, they are used for the normalisation.
#'
#' @template arg_data
#'
#' @param to.range Normalise values to this range. If the objective is
#'   maximised, it is normalised to \code{c(to.range[1], to.range[0])}
#'   instead.
#'
#' @param lower,upper Bounds on the values. If NA, the maximum and minimum
#'   values of each coordinate are used.
#'
#' @template arg_maximise
#'
#' @return  A numerical matrix
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @examples
#'
#' data(SPEA2minstoptimeRichmond)
#' # The second objective must be maximized
#' head(SPEA2minstoptimeRichmond[, 1:2])
#'
#' head(normalise(SPEA2minstoptimeRichmond[, 1:2], maximise = c(FALSE, TRUE)))
#'
#' head(normalise(SPEA2minstoptimeRichmond[, 1:2], to.range = c(0,1), maximise = c(FALSE, TRUE)))
#' 
#' @export
normalise <- function(data, to.range = c(1, 2), lower = NA, upper = NA, maximise = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data)
  npoints <- nrow(data)
  lower <- as.double(rep_len(lower, nobjs))
  upper <- as.double(rep_len(upper, nobjs))
  # Handle NA
  minmax <- apply(data, 2, range)
  no.lower <- is.na(lower)
  no.upper <- is.na(upper)
  lower[no.lower] <- minmax[1, no.lower]
  upper[no.upper] <- minmax[2, no.upper]
  maximise <- as.logical(rep_len(maximise, nobjs))

  if (length(to.range) != 2L)
    stop("to.range must be a vector of length 2")
  
  z <- t(.Call(normalise_C,
                 as.double(t(data)),
                 as.integer(nobjs),
                 as.integer(npoints),
                 as.double(to.range),
                 lower, upper, maximise))
  colnames(z) <- colnames(data)
  rownames(z) <- rownames(data)
  return(z)
}

