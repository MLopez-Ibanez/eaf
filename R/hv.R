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
#' C. M. Fonseca, L. Paquete, and M. López-Ibáñez. An improved dimension-sweep
#' algorithm for the hypervolume indicator. In IEEE Congress on Evolutionary
#' Computation, pages 1157-1163, Vancouver, Canada, July 2006.
#'
#' Nicola Beume, Carlos M. Fonseca, Manuel López-Ibáñez, Luís Paquete, and
#' J. Vahrenhold. On the complexity of computing the hypervolume indicator.
#' IEEE Transactions on Evolutionary Computation, 13(5):1075-1082, 2009.
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
  if (is.null(reference)) {
    stop("reference cannot be NULL")
  }
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
  return(.Call("hypervolume_C",
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
#' C. M. Fonseca, L. Paquete, and M. López-Ibáñez. An improved dimension-sweep
#' algorithm for the hypervolume indicator. In IEEE Congress on Evolutionary
#' Computation, pages 1157-1163, Vancouver, Canada, July 2006.
#'
#' Nicola Beume, Carlos M. Fonseca, Manuel López-Ibáñez, Luís Paquete, and
#' J. Vahrenhold. On the complexity of computing the hypervolume indicator.
#' IEEE Transactions on Evolutionary Computation, 13(5):1075-1082, 2009.
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
  if (is.null(reference)) {
    stop("reference cannot be NULL")
  }
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
  return(.Call("hv_contributions_C",
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
#' Given objective vectors a and b, epsilon(a,b) is computed in the case of
#' minimization as a/b for the multiplicative variant (respectively, a - b for
#' the additive variant), whereas in the case of maximization it is computed as
#' b/a for the multiplicative variant (respectively, b - a for the additive
#' variant). This allows computing a single value for mixed optimization
#' problems, where some objectives are to be maximized while others are to be
#' minimized. Moreover, a lower value corresponds to a better approximation
#' set, independently of the type of problem (minimization, maximization or
#' mixed). However, the meaning of the value is different for each objective
#' type. For example, imagine that f1 is to be minimized and f2 is to be
#' maximized, and the multiplicative epsilon computed here for epsilon(A,B) =
#' 3. This means that A needs to be multiplied by 1/3 for all f1 values and by
#' 3 for all f2 values in order to weakly dominate B.
#'
#' This also means that the computation of the multiplicative version for
#' negative values doesn't make sense.
#'
#' @references
#'
#' E. Zitzler, L. Thiele, M. Laumanns, C. M. Fonseca, and V. Grunert da
#' Fonseca.  Performance Assessment of Multiobjective Optimizers: an Analysis
#' and Review. IEEE Transactions on Evolutionary Computation, 7(2):117-132,
#' 2003.
#' 
#' @examples
#' extdata_path <- system.file(package="eaf","extdata")
#' path.A1 <- file.path(extdata_path, "ALG_1_dat")
#' path.A2 <- file.path(extdata_path, "ALG_2_dat")
#' A1 <- read_datasets(path.A1)[,1:2]
#' A2 <- read_datasets(path.A2)[,1:2]
#' ref <- filter_dominated(rbind(A1, A2))
#' epsilon_additive(A1, ref)
#' epsilon_additive(A2, ref)
#' 
#' @rdname epsilon
#' @export
epsilon_additive <- function(data, reference, maximise = FALSE)
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
    
  return(.Call("epsilon_add_C",
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(t(reference)),
               as.integer(reference_size),
               maximise))
}

#' @rdname epsilon
#' @export
#' @examples
#' # Multiplicative version of epsilon metric
#' ref <- filter_dominated(rbind(A1, A2))
#' epsilon_mult(A1, ref)
#' epsilon_mult(A2, ref)
#' 
epsilon_mult <- function(data, reference, maximise = FALSE)
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
    
  return(.Call("epsilon_mul_C",
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(t(reference)),
               as.integer(reference_size),
               maximise))
}

#' Inverted Generational Distance (IGD and IGD+)
#'
#' Computes the inverted generational distance (IGD and IGD+)
#'
#' @rdname igd
#' @export
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
#' GD was first proposed by Van Veldhuizen and Lamont (1997)in [1] with
#' p=2. IGD seems to have been mentioned first by Coello Coello & Reyes-Sierra
#' (2004), however, some people also used the name D-metric for the same thing
#' with p=1 and later papers have often used IGD/GD with p=1.
#'
#' The modified inverted generational distanced (IGD+) was proposed by
#' Ishibuchi et all. (2015) to ensure that IGD+ is weakly Pareto compliant,
#' similarly to unary epsilon. It averages over \eqn{|R|} within the exponent
#' \eqn{1/p} and modifies the distance measure as:
#'
#' \deqn{d^+(r,a) = \sqrt{\sum_{k=1}^M (\max\{r_k - a_k, 0\})^2}}{d^+(r,a) = sqrt(sum_{k=1}^M (max {r_k - a_k, 0 })^2)}
#'
#' See Bezerra et al. (2017) for a comparison.
#' 
#' @references
#'
#' D. A. Van Veldhuizen and G. B. Lamont. Evolutionary Computation and
#' Convergence to a Pareto Front. In J. R. Koza, editor, Late Breaking Papers
#' at the Genetic Programming 1998 Conference, pages 221–228, Stanford
#' University, California, July 1998. Stanford University Bookstore.  Keywords:
#' generational distance.
#'
#' Coello Coello, C.A., Reyes-Sierra, M.: A study of the parallelization of a
#' coevolutionary multi-objective evolutionary algorithm.  In: Monroy, R., et
#' al. (eds.) Proceedings of MICAI, LNAI, vol. 2972, pp. 688–697. Springer,
#' Heidelberg, Germany (2004).
#'
#' Q. Zhang and H. Li. MOEA/D: A Multiobjective Evolutionary Algorithm Based
#' on Decomposition. IEEE Transactions on Evolutionary Computation,
#' 11(6):712–731, 2007. doi:10.1109/TEVC.2007.892759.
#'
#' Schutze, O., Esquivel, X., Lara, A., Coello Coello, C.A.: Using the
#' averaged Hausdorff distance as a performance measure in evolutionary
#' multiobjective optimization. IEEE Trans. Evol. Comput. 16(4), 504–522 (2012)
#'
#' H. Ishibuchi, H. Masuda, Y. Tanigaki, and Y. Nojima.  Modified Distance
#' Calculation in Generational Distance and Inverted Generational Distance.
#' In A. Gaspar-Cunha, C. H. Antunes, and C. A. Coello Coello, editors,
#' Evolutionary Multi-criterion Optimization, EMO 2015 Part I, volume 9018 of
#' Lecture Notes in Computer Science, pages 110–125. Springer, Heidelberg,
#' Germany, 2015.
#'
#' Leonardo C. T. Bezerra, Manuel López-Ibáñez, and Thomas Stützle. An
#' empirical assessment of the properties of inverted generational distance
#' indicators on multi- and many-objective optimization. In H. Trautmann,
#' G. Rudolph, K. Klamroth, O. Schütze, M. M. Wiecek, Y. Jin, and C. Grimme,
#' editors, Evolutionary Multi-criterion Optimization, EMO 2017, Lecture
#' Notes in Computer Science, pages 31–45. Springer, 2017.
#' 
#' @examples
#' extdata_path <- system.file(package="eaf","extdata")
#' path.A1 <- file.path(extdata_path, "ALG_1_dat")
#' path.A2 <- file.path(extdata_path, "ALG_2_dat")
#' A1 <- read_datasets(path.A1)[,1:2]
#' A2 <- read_datasets(path.A2)[,1:2]
#' ref <- filter_dominated(rbind(A1, A2))
#' igd(A1, ref)
#' igd(A2, ref)
#' 
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
    
  return(.Call("igd_C",
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
#' ref <- filter_dominated(rbind(A1, A2))
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
    
  return(.Call("igd_plus_C",
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(t(reference)),
               as.integer(reference_size),
               maximise))
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
  return(t(.Call("normalise_C",
                 as.double(t(data)),
                 as.integer(nobjs),
                 as.integer(npoints),
                 as.double(to.range),
                 lower, upper, maximise)))
}
