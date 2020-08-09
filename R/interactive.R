#' Interactively choose according to empirical attainment function differences 
#' 
#' The function `choose_eafdiffplot()` creates the same plot as `eafdiffplot()`
#' but waits for the user to click in one of the sides. Then it returns the
#' rectangles the give the differences in favour of the chosen side. These
#' rectangles may be used for interactive decision-making as shown in
#' \citet{DiaLop2020ejor}. The function `choose_eafdiff()` may be used in a
#' non-interactive context.
#' 
#' @param data.left,data.right Data frames corresponding to the input data of
#'   left and right sides, respectively. Each data frame has at least three
#'   columns, the third one being the set of each point. See also
#'   [read_datasets()].
#' 
#' @param intervals (`integer(1)`|`character()`) \cr The absolute range of the
#'   differences \eqn{[0, 1]} is partitioned into the number of intervals
#'   provided. If an integer is provided, then labels for each interval are
#'   computed automatically. If a character vector is provided, its length is
#'   taken as the number of intervals.
#' 
#' @template arg_maximise
#'
#' @param title.left,title.right Title for left and right panels, respectively.
#'  
#' @param ... Other graphical parameters are passed down to
#'   [eafdiffplot()].
#' 
#' 
#' @return `matrix` where the first 4 columns give the coordinates of two
#'   corners of each rectangle and the last column. In both cases, the last
#'   column gives the positive differences in favor of the chosen side.
#' 
#' @seealso    [read_datasets()], [eafdiffplot()], [whv_rect()]
#' 
#' @examples
#'
#' \donttest{
#' extdata_dir <- system.file(package="eaf", "extdata") 
#' A1 <- read_datasets(file.path(extdata_dir, "wrots_l100w10_dat"))
#' A2 <- read_datasets(file.path(extdata_dir, "wrots_l10w100_dat"))
#' if (interactive()) {
#'   rectangles <- choose_eafdiffplot(A1, A2, intervals = 5)
#' } else { # Choose A1
#'   rectangles <- eafdiff(A1, A2, intervals = 5, rectangles = TRUE)
#'   rectangles <- choose_eafdiff(rectangles, left = TRUE)
#' }
#' reference <- c(max(A1[, 1], A2[, 1]), max(A1[, 2], A2[, 2]))
#' x <- split.data.frame(A1[,1:2], A1[,3])
#' hv_A1 <- sapply(split.data.frame(A1[, 1:2], A1[, 3]),
#'                  hypervolume, reference=reference)
#' hv_A2 <- sapply(split.data.frame(A2[, 1:2], A2[, 3]),
#'                  hypervolume, reference=reference)
#' boxplot(list(A1=hv_A1, A2=hv_A2), main = "Hypervolume")
#'
#' whv_A1 <- sapply(split.data.frame(A1[, 1:2], A1[, 3]),
#'                  whv_rect, rectangles=rectangles, reference=reference)
#' whv_A2 <- sapply(split.data.frame(A2[, 1:2], A2[, 3]),
#'                  whv_rect, rectangles=rectangles, reference=reference)
#' boxplot(list(A1=whv_A1, A2=whv_A2), main = "Weighted hypervolume")
#' }
#'@keywords graphs
#'
#'@references
#' \insertAllCited{}
#' @export
#'@md
choose_eafdiffplot <- function(data.left, data.right, intervals = 5,
                               maximise = c(FALSE, FALSE),
                               title.left = deparse(substitute(data.left)),
                               title.right = deparse(substitute(data.right)),
                               ...)
{
  op <- options(locatorBell = FALSE)
  on.exit(options(op))
  eafdiffplot(data.left, data.right, title.left= title.left, title.right = title.right,
              intervals = intervals, maximise = maximise, ...)
  # FIXME: Avoid calculating eafdiff twice.
  DIFF <- eafdiff(data.left, data.right, intervals = intervals, maximise = maximise,
                  rectangles = TRUE)

  coord <- grid::grid.locator("npc")
  
  if (coord$x[[1]] < 0.5) {
    cat("LEFT!\n")
    return (choose_eafdiff(DIFF, left=TRUE))
  } else {
    cat("RIGHT!\n")
    return (choose_eafdiff(DIFF, left=FALSE))
  }
}

#' Identify largest EAF differences
#' 
#' Given a list of datasets, return the indexes of the pair with the largest
#' EAF differences according to the method proposed by \citet{DiaLop2020ejor}.
#' 
#'
#' @param data (`list(1)`) A list of matrices with at least 3 columns
#'
#' @template arg_maximise
#'
#' @param intervals (`integer(1)`) \cr The absolute range of the differences
#'   \eqn{[0, 1]} is partitioned into the number of intervals provided.
#'
#' @template arg_refpoint
#'
#' @template arg_ideal
#'
#' @return  (`list()`) A list with two components `best_pair` and `best_value`.
#' 
#'@examples
#' # FIXME: This example is too large, we need a smaller one.
#' files <- c("wrots_l100w10_dat","wrots_l10w100_dat")
#' data <- lapply(files, function(x)
#'                read_datasets(file.path(system.file(package="eaf"),
#'                              "extdata", x)))
#' nadir <- apply(do.call(rbind, data)[,1:2], 2, max)
#' x <- largest_eafdiff(data, reference = nadir)
#' str(x)
#'
#'@references
#' \insertAllCited{}
#' 
#'@export
#'@md
largest_eafdiff <- function(data, maximise = FALSE, intervals = 5, reference,
                            ideal = NULL)
{
  nobjs <- 2
  maximise <- as.logical(rep_len(maximise, nobjs))
  if (nobjs != 2) {
    stop("sorry: only 2 objectives supported")
  }
  n <- length(data)
  stopifnot(n > 1)
  best_pair <- NULL
  best_value <- 0
  if (is.null(ideal)) {
    # or do.call(rbind,), but that consumes lots of memory
    minmax <- apply(sapply(data, function(x) apply(x[,1:2], 2, range)), 2, range)
    lower <- minmax[1,]
    upper <- minmax[2,]
    ideal <- ifelse(maximise, upper, lower)
  }
  # Convert to a 1-row matrix
  if (is.null(dim(ideal))) dim(ideal) <- c(1,nobjs)
    
  for (a in 1:(n-1)) {
    for (b in (a+1):n) {
      DIFF <- eafdiff(data[[a]], data[[b]], intervals = intervals,
                      maximise = maximise, rectangles = TRUE)
      # Set color to 1
      a_rectangles <- DIFF[ DIFF[, 5] >= 1L, , drop = FALSE]
      a_rectangles[, ncol(a_rectangles)] <- 1
      
      a_value <- whv_rect(ideal, a_rectangles, reference, maximise)
      
      b_rectangles <- DIFF[ DIFF[, 5] <= -1L, , drop = FALSE]
      b_rectangles[, ncol(b_rectangles)] <- 1
      b_value <- whv_rect(ideal, b_rectangles, reference, maximise)
      value <- min(a_value, b_value)
      if (value > best_value) {
        best_value <- value
        best_pair <- c(a, b)
      }
    }
  }
  return(list(pair=best_pair, value = best_value))
}


#' @param x (`matrix()`) Matrix of rectangles representing EAF differences
#'   (returned by [eafdiff()] with `rectangles=TRUE`).
#' 
#' @param left (`logical(1)`) With `left=TRUE` return the rectangles with
#'   positive differences, otherwise return those with negative differences but
#'   differences are converted to positive.
#' 
#' @rdname choose_eafdiffplot
#'@export
#'@md
choose_eafdiff <- function(x, left = stop("'left' must be either TRUE or FALSE"))
{
  if (left) return (x[ x[, ncol(x)] > 0L, , drop = FALSE])
  x <- x[ x[, ncol(x)] < 0L, , drop = FALSE]
  # We always return positive colors.
  x[, ncol(x)] <- abs(x[, ncol(x)])
  return(x)
}

