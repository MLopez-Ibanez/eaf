
check.hv.data <- function(x)
{
  name <- deparse(substitute(x))
  if (length(dim(x)) != 2L)
    stop("'", name, "' must be a data.frame or a matrix")
  if (nrow(x) < 1L)
    stop("not enough points (rows) in '", name, "'")
  if (ncol(x) < 2)
    stop("'", name, "' must have at least 2 columns")
  x <-  as.matrix(x)
  if (!is.numeric(x))
    stop("'", name, "' must be numeric")
  return(x)
}

#' Hypervolume metric
#'
#' Computes the hypervolume metric with respect to a given reference point
#' assuming minimization of all objectives.
#'
#' @param data Either a matrix or a data frame of numerical values, where
#'   each row gives the coordinates of a point.
#'
#' @param reference Reference point as a vector of numerical values. 
#'
#' @param maximise Whether the objectives must be maximised instead of
#'   minimised. Either a single boolean value that applies to all objectives or
#'   a vector boolean values, with one value per objective.
#' 
#' @return  A single numerical value.
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#'@seealso \code{\link{read.table}}
#'
#' @references
#'
#'  C. M. Fonseca, L. Paquete, and M. López-Ibáñez. An improved dimension-sweep
#'  algorithm for the hypervolume indicator. In IEEE Congress on Evolutionary
#'  Computation, pages 1157-1163, Vancouver, Canada, July 2006.
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
  data <- check.hv.data(data)
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
  return(.Call("hypervolume_C",
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               as.double(reference)
               ))
}

#' Hypervolume contribution of a set of points
#'
#' Computes the hypervolume contribution of each point given a set of points
#' with respect to a given reference point assuming minimization of all
#' objectives.
#'
#' @param data Either a matrix or a data frame of numerical values, where
#'   each row gives the coordinates of a point.
#'
#' @param reference Reference point as a vector of numerical values. 
#'
#' @param maximise Whether the objectives must be maximised instead of
#'   minimised. Either a single boolean value that applies to all objectives or
#'   a vector boolean values, with one value per objective.
#' 
#' @return  A numerical vector
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#'@seealso \code{\link{read.table}} \code{\link{hypervolume}}
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
#' @export
hv_contributions <- function(data, reference, maximise = FALSE)
{
  data <- check.hv.data(data)
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

#' Normalise points
#'
#' Normalise points per coordinate to a range, e.g., \code{c(1,2)}, where the
#' minimum value will correspond to 1 and the maximum to 2. If bounds are
#' given, they are used for the normalisation.
#'
#' @param data Either a matrix or a data frame of numerical values, where
#'   each row gives the coordinates of a point.
#'
#' @param to.range Normalise values to this range.
#'
#' @param lower,upper Bounds on the values. If NA, the maximum and minimum
#'   values of each coordinate are used.
#'
#' @param maximise Whether the objectives must be maximised instead of
#'   minimised. Either a single boolean value that applies to all objectives or
#'   a vector boolean values, with one value per objective. A true value means
#'   that the corresponding objective is normalised to \code{c(to.range[1],
#'   to.range[0])} instead.
#'
#' @return  A numerical matrix
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @examples
#'
#' data(SPEA2minstoptimeRichmond)
#' # The second objective must be maximized
#' # We calculate the hypervolume of the union of all sets.
#' normalise(SPEA2minstoptimeRichmond[, 1:2], maximise = c(FALSE, TRUE))
#' normalise(SPEA2minstoptimeRichmond[, 1:2], to.range = c(0,1), maximise = c(FALSE, TRUE))
#'
#' 
#' @export
normalise <- function(data, to.range = c(1, 2), lower = NA, upper = NA, maximise = FALSE)
{
  data <- check.hv.data(data)
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
