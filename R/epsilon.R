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
