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
