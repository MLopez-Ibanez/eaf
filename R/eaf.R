###############################################################################
#
#                       Copyright (c) 2011-2019
#         Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>
#                Marco Chiarandini <marco@imada.sdu.dk>
#
# This program is free software (software libre); you can redistribute
# it and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, you can obtain a copy of the GNU
# General Public License at http://www.gnu.org/copyleft/gpl.html
#
# IMPORTANT NOTE: Please be aware that the fact that this program is
# released as Free Software does not excuse you from scientific
# propriety, which obligates you to give appropriate credit! If you
# write a scientific paper describing research that made substantive use
# of this program, it is your obligation as a scientist to (a) mention
# the fashion in which this software was used in the Methods section;
# (b) mention the algorithm in the References section. The appropriate
# citation is:
# 
#    Manuel Lopez-Ibanez, Luis Paquete, and Thomas Stuetzle.
#    Exploratory Analysis of Stochastic Local Search Algorithms in
#    Biobjective Optimization. In T. Bartz-Beielstein, M. Chiarandini,
#    L. Paquete, and M. Preuss, editors, Experimental Methods for the
#    Analysis of Optimization Algorithms, pages 209-222. Springer,
#    Berlin, Germany, 2010. doi: 10.1007/978-3-642-02538-9_9
# 
# Moreover, as a personal note, I would appreciate it if you would email
# manuel.lopez-ibanez@manchester.ac.uk with citations of papers referencing
# this work so I can mention them to my funding agent and tenure committee.
#
################################################################################
#
# TODO:
#
#  * Follow this style for coding:
#    http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################################################################################
#dyn.load("../src/eaf.so")

check.eaf.data <- function(x)
{
  name <- deparse(substitute(x))
  if (length(dim(x)) != 2L)
    stop("'", name, "' must be a data.frame or a matrix")
  if (nrow(x) < 1L)
    stop("not enough points (rows) in '", name, "'")
  if (ncol(x) < 3)
    stop("'", name, "' must have at least 3 columns: 2D points and set index")
  # Re-encode the sets so that they are consecutive and numeric
  setcol <- ncol(x)
  x[, setcol] <- as.numeric(as.factor(x[, setcol]))
  x <-  as.matrix(x)
  if (!is.numeric(x))
    stop("The two first columns of '", name, "' must be numeric")
  return(x)
}

compute.eaf <- function(data, percentiles = NULL)
{
  data <- check.eaf.data(data)
  setcol <- ncol(data)
  nobjs <- setcol - 1L
  # The C code expects points within a set to be contiguous.
  data <- data[order(data[, setcol]), , drop=FALSE]
  sets <- data[, setcol]
  nsets <- length(unique(sets))
  npoints <- tabulate(sets)
  if (is.null(percentiles)) {
    # FIXME: We should probably compute this in the C code.
    percentiles <- 1L:nsets * 100.0 / nsets
  }
  percentiles <- unique.default(sort.int(percentiles))
  return(.Call(compute_eaf_C,
               as.double(t(as.matrix(data[, 1L:nobjs]))),
               nobjs,
               as.integer(cumsum(npoints)),
               as.integer(nsets),
               percentiles))
}

compute.eaf.as.list <- function(data, percentiles = NULL)
{
  eaf <- compute.eaf (data, percentiles = percentiles)
  setcol <- ncol(eaf)
  nobjs <- setcol - 1L
  eaf_sets <- eaf[, setcol]
  uniq_eaf_sets <- unique.default(eaf[, setcol])
  return(split.data.frame(eaf[,1:nobjs, drop=FALSE],
                          factor(eaf_sets,
                                 levels = uniq_eaf_sets,
                                 labels = uniq_eaf_sets)))
}

compute.eafdiff.helper <- function(data, intervals)
{
  # Last column is the set number.
  setcol <- ncol(data)
  nobjs <- setcol - 1L
  # the C code expects points within a set to be contiguous.
  data <- data[order(data[, setcol]), ]
  sets <- data[, setcol]
  nsets <- length(unique(sets))
  npoints <- tabulate(sets)
  # FIXME: Ideally this would be computed by the C code, but it is hard-coded.
  ## division <- nsets %/% 2
  ## nsets1 <- division
  ## nsets2 <- nsets - division
  return(.Call(compute_eafdiff_C,
                as.double(t(as.matrix(data[, 1L:nobjs]))),
                nobjs,
                as.integer(cumsum(npoints)),
                as.integer(nsets),
                as.integer(intervals)))
}

#' Compute empirical attainment function differences 
#' 
#' Calculate the differences between the empirical attainment functions of two
#' data sets.
#' 
#' @param x,y Data frames corresponding to the input data of
#'   left and right sides, respectively. Each data frame has at least three
#'   columns, the third one being the set of each point. See also
#'   [read_datasets()].
#'
#' @param intervals (`integer(1)`) \cr The absolute range of the differences
#'   \eqn{[0, 1]} is partitioned into the number of intervals provided.
#' 
#' @template arg_maximise
#'
#' @param rectangles If TRUE, the output is in the form of rectangles of the same color.
#' 
#' @details
#'   This function calculates the differences between the EAFs of two
#'   data sets.
#'
#' @return With `rectangle=FALSE`, a `data.frame` containing points where there
#'   is a transition in the value of the EAF differences.  With
#'   `rectangle=TRUE`, a `matrix` where the first 4 columns give the
#'   coordinates of two corners of each rectangle and the last column. In both
#'   cases, the last column gives the difference in terms of sets in `x` minus
#'   sets in `y` that attain each point (i.e., negative values are differences
#'   in favour `y`).
#' 
#' @seealso    [read_datasets()], [eafdiffplot()]
#' 
#' @examples
#'
#' A1 <- read_datasets(text='
#'  3 2
#'  2 3
#'  
#'  2.5 1
#'  1 2
#'  
#'  1 2
#' ')
#' A2 <- read_datasets(text='
#'  4 2.5
#'  3 3
#'  2.5 3.5
#'  
#'  3 3
#'  2.5 3.5
#'  
#'  2 1
#' ')
#' d <- eafdiff(A1, A2)
#' str(d)
#' print(d)
#'
#' d <- eafdiff(A1, A2, rectangles = TRUE)
#' str(d)
#' print(d)
#'
#'@export
#'@md
eafdiff <- function(x, y, intervals = NULL, maximise = c(FALSE, FALSE),
                    rectangles = FALSE)
{
  maximise <- as.logical(maximise)
  nsets <- (length(unique(x[,ncol(x)])) + length(unique(y[,ncol(y)])))
  if (is.null(intervals)) {
    # Default is nsets / 2
    intervals <- nsets / 2.0
  } else {
    stopifnot(length(intervals) == 1L)
    intervals <- min(intervals, nsets / 2.0)
  }

  data <- rbind_datasets(x, y)
  data <- check.eaf.data(data)
  # FIXME: Is it faster to subset or to multiply the third column by 1?
  data[,1:2] <- matrix.maximise(data[,1:2, drop=FALSE], maximise = maximise)
  
  DIFF <- if (rectangles) compute.eafdiff.rectangles(data, intervals = intervals)
          else compute.eafdiff.helper(data, intervals = intervals)
  # FIXME: We should remove duplicated rows in C code.

  # FIXME: Check that we do not generate duplicated nor overlapping rectangles
  # with different colors. That would be a bug.
  DIFF <- DIFF[!duplicated(DIFF),]
  return(DIFF)
}

compute.eafdiff <- function(data, intervals)
{
  DIFF <- compute.eafdiff.helper(data, intervals)
  #print(DIFF)
  # FIXME: Do this computation in C code.
  setcol <- ncol(data)
  eafval <- DIFF[, setcol]
  eafdiff <- list(left  = unique(DIFF[ eafval >= 1L, , drop=FALSE]),
                  right = unique(DIFF[ eafval <= -1L, , drop=FALSE]))
  eafdiff$right[, setcol] <- -eafdiff$right[, setcol]
  return(eafdiff)
}


# FIXME: The default intervals should be nsets / 2
compute.eafdiff.rectangles <- function(data, intervals = 1L)
{
  # Last column is the set number.
  nobjs <- ncol(data) - 1L
  # the C code expects points within a set to be contiguous.
  data <- data[order(data[, nobjs + 1L]), ]
  sets <- data[ , nobjs + 1L]
  nsets <- length(unique(sets))
  npoints <- tabulate (sets)
  return(.Call(compute_eafdiff_rectangles_C,
               as.double(t(as.matrix(data[, 1L:nobjs]))),
               nobjs,
               as.integer(cumsum(npoints)),
               as.integer(nsets),
               as.integer(intervals)))
}

# FIXME: The default intervals should be nsets / 2
compute.eafdiff.polygon <- function(data, intervals = 1L)
{
  # Last column is the set number.
  nobjs <- ncol(data) - 1L
  # the C code expects points within a set to be contiguous.
  data <- data[order(data[, nobjs + 1L]), ]
  sets <- data[ , nobjs + 1L]
  nsets <- length(unique(sets))
  npoints <- tabulate (sets)
  # FIXME: Ideally this would be computed by the C code, but it is hard-coded.
  ## division <- nsets %/% 2
  ## nsets1 <- division
  ## nsets2 <- nsets - division
  # FIMXE: This function may require a lot of memory for 900 sets. Is there a
  # way to save memory?
  return(.Call(compute_eafdiff_area_C,
               as.double(t(as.matrix(data[, 1L:nobjs]))),
               nobjs,
               as.integer(cumsum(npoints)),
               as.integer(nsets),
               as.integer(intervals)))
}


rm.inf <- function(x, xmax)
{
  x[is.infinite(x)] <- xmax
  return(x)
}


# FIXME: Accept ...
max.finite <- function (x)
{
  x <- as.vector(x)
  x <- x[is.finite(x)]
  if (length(x)) return(max(x))
  return(NULL)
}

# FIXME: Accept ...
min.finite <- function (x)
{
  x <- as.vector(x)
  x <- x[is.finite(x)]
  if (length(x)) return(min(x))
  return(NULL)
}

# FIXME: Accept ...
range.finite <- function(x)
{
  return(c(min.finite(x), max.finite(x)))
}

matrix.maximise <- function(z, maximise)
{
  stopifnot(ncol(z) == length(maximise))
  if (is.data.frame(z)) {
    # R bug?: If z is data.frame with rownames != NULL, and
    # maximise == (FALSE, FALSE), then -z[, which(FALSE)]
    # gives error: Error in
    # data.frame(value, row.names = rn, check.names = FALSE, check.rows = FALSE) : 
    # row names supplied are of the wrong length
    row_names <- rownames(z)
    rownames(z) <- NULL
    x <- which(maximise)
    z[, x] <- -z[, x]
    rownames(z) <- row_names
  } else {
    x <- ifelse(maximise, -1L, 1L)
    z <- t(t(z) * x)
  }
  return(z)
}

#' Read several data sets
#'
#' Reads a text file in table format and creates a matrix from it. The file
#' may contain several sets, separated by empty lines. Lines starting by
#' `'#'` are considered comments and treated as empty lines. The function
#' adds an additional column `set` to indicate to which set each row
#' belongs.
#'
#' @param file (`character()`) \cr Filename that contains the data.  Each row
#'   of the table appears as one line of the file.  If it does not contain an
#'   \emph{absolute} path, the file name is \emph{relative} to the current
#'   working directory, \code{\link[base]{getwd}()}.  Tilde-expansion is
#'   performed where supported.  Files compressed with `xz` are supported.
#'
#' @param col_names,col.names Vector of optional names for the variables.  The
#'   default is to use \samp{"V"} followed by the column number.
#'
#' @param text (`character()`) \cr If `file` is not supplied and this is,
#'   then data are read from the value of `text` via a text connection.
#'   Notice that a literal string can be used to include (small) data sets
#'   within R code.
#'
#' @return  (`matrix()`) containing a representation of the
#'  data in the file. An extra column `set` is added to indicate to
#'  which set each row belongs. 
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @note There are several examples of data sets in
#'   `system.file(package="eaf","extdata")`.
#'
#' `read.data.sets()` is a deprecated alias. It will be removed in the next
#'   major release.
#' 
#' @section Warning:
#'  A known limitation is that the input file must use newline characters
#'  native to the host system, otherwise they will be, possibly silently,
#'  misinterpreted. In GNU/Linux the program `dos2unix` may be used
#'  to fix newline characters.
#'
#'@seealso \code{\link[utils]{read.table}}, [eafplot()], [eafdiffplot()]
#'
#'@examples
#' extdata_path <- system.file(package="eaf","extdata")
#' A1 <- read_datasets(file.path(extdata_path,"ALG_1_dat.xz"))
#' str(A1)
#' A2 <- read_datasets(file.path(extdata_path,"ALG_2_dat.xz"))
#' str(A2)
#'
#' read_datasets(text="1 2\n3 4\n\n5 6\n7 8\n", col_names=c("obj1", "obj2"))
#' 
#' @keywords file
#' @md
#' @export
read_datasets <- function(file, col_names, text)
{
  if (missing(file) && !missing(text)) {
    file <- tempfile()
    writeLines(text, file)
    on.exit(unlink(file))
  } else {
    if (!file.exists(file))
      stop("error: ", file, ": No such file or directory");
    file <- normalizePath(file)
    if (grepl("\\.xz$", file)) {
      unc_file <- tempfile()
      writeLines(readLines(zz <- xzfile(file, "r")), unc_file)
      close(zz)
      file <- unc_file
      on.exit(unlink(file))
    }
  }
  out <- .Call(read_data_sets, as.character(file))
  if (missing(col_names))
    col_names <- paste0("V", 1L:(ncol(out)-1))
  colnames(out) <- c(col_names, "set")
  return(out)
}

#' @rdname read_datasets
#' @export
read.data.sets <- function(file, col.names)
{
  .Deprecated("read_datasets")
  return(read_datasets(file=file, col_names=col.names))
}

rbind_datasets <- function(x,y)
{
  stopifnot(min(x[,3]) == 1)
  stopifnot(min(y[,3]) == 1)
  # We have to make all sets unique.
  y[,3] <- y[,3] + max(x[,3])
  return(rbind(x, y))
}

## Calculate the intermediate points in order to plot a staircase-like
## polygon.
## Example: given ((1,2), (2,1)), it returns ((1,2), (2,2), (2,1)).
## Input should be already in the correct order.
points.steps <- function(x)
{
  n <- nrow(x)
  if (n == 1L) return(x)
  x <- rbind(x, cbind(x=x[-1L, 1L, drop=FALSE], y=x[-n, 2L, drop=FALSE]))
  idx <- c(as.vector(outer(c(0L, n), 1L:(n - 1L), "+")), n)
  stopifnot(length(idx) == nrow(x))
  stopifnot(!anyDuplicated(idx))
  return(x[idx, ])
}



#' Exact computation of the EAF
#'
#' This function computes the EAF given a set of points and a vector `set`
#' that indicates to which set each point belongs.
#'
#' @param points Either a matrix or a data frame of numerical values, where
#'   each row gives the coordinates of a point.
#' 
#' @param sets A vector indicating which set each point belongs to.
#'
#' @param groups Indicates that the EAF must be computed separately for data
#'   belonging to different groups.
#'
#' @param percentiles The percentiles of the EAF of each side that will be
#'   plotted as attainment surfaces. `NA` does not plot any. See
#'   [eafplot()].
#'
#' @return  A data frame (`data.frame`) containing the exact representation
#'  of EAF. The last column gives the percentile that corresponds to each
#'  point. If groups is not `NULL`, then an additional column
#'  indicates to which group the point belongs.
#'
#' @author  Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#'@note There are several examples of data sets in `system.file(package="eaf","extdata")`.
#'
#'@seealso [read_datasets()]
#'
#'@examples
#' extdata_path <- system.file(package="eaf", "extdata")
#' 
#' x <- read_datasets(file.path(extdata_path, "example1_dat"))
#' # Compute full EAF
#' str(eafs(x[,1:2], x[,3]))
#' 
#' # Compute only best, median and worst
#' str(eafs(x[,1:2], x[,3], percentiles = c(0, 50, 100)))
#'
#' x <- read_datasets(file.path(extdata_path, "spherical-250-10-3d.txt"))
#' y <- read_datasets(file.path(extdata_path, "uniform-250-10-3d.txt"))
#' x <- data.frame(x, groups = "spherical")
#' x <- rbind(x, data.frame(y, groups = "uniform"))
#' # Compute only median separately for each group
#' z <- eafs(x[,1:3], sets = x[,4], groups = x[,5], percentiles = 50)
#' str(z)
#' # library(plotly)
#' # plot_ly(z, x = ~X1, y = ~X2, z = ~X3, color = ~groups,
#' #         colors = c('#BF382A', '#0C4B8E')) %>% add_markers()
#'@export
#'@md
eafs <- function (points, sets, groups = NULL, percentiles = NULL)
{
  points <- cbind(points, sets)

  if (is.null(groups)) {
    attsurfs <- compute.eaf (points, percentiles)
  } else {
    attsurfs <- data.frame()
    groups <- factor(groups)
    for (g in levels(groups)) {
      tmp <- compute.eaf(points[groups == g,], percentiles)
      attsurfs <- rbind(attsurfs, data.frame(tmp, groups = g))
    }
  }
  return (attsurfs)
}


# Get correct xlim or ylim when maximising / minimising.
get.xylim <- function(lim, maximise, data)
{
  # FIXME: This seems too complicated.
  if (!is.null(lim) && maximise) lim <- -lim 
  if (is.null(lim)) lim <- range(data)
  if (maximise) lim <- range(-lim)
  return(lim)
}
  
get.extremes <- function(xlim, ylim, maximise, log)
{
  if (length(log) && log != "")
    log <- strsplit(log, NULL)[[1L]]
  if ("x" %in% log) xlim <- log(xlim)
  if ("y" %in% log) ylim <- log(ylim)
  
  extreme1 <- ifelse(maximise[1],
                     xlim[1] - 0.05 * diff(xlim),
                     xlim[2] + 0.05 * diff(xlim))
  extreme2 <- ifelse(maximise[2],
                     ylim[1] - 0.05 * diff(ylim),
                     ylim[2] + 0.05 * diff(ylim))
  
  if ("x" %in% log) extreme1 <- exp(extreme1)
  if ("y" %in% log) extreme2 <- exp(extreme2)
  return(c(extreme1, extreme2))
}

#' Plot the Empirical Attainment Function for two objectives
#' 
#' Computes and plots the Empirical Attainment Function, either as
#' attainment surfaces for certain percentiles or as points.
#'
#' This function can be used to plot random sets of points like those obtained
#' by different runs of biobjective stochastic optimization algorithms.  An EAF
#' curve represents the boundary separating points that are known to be
#' attainable (that is, dominated in Pareto sense) in at least a fraction
#' (quantile) of the runs from those that are not. The median EAF represents
#' the curve where the fraction of attainable points is 50\%.  In single
#' objective optimization the function can be used to plot the profile of
#' solution quality over time of a collection of runs of a stochastic optimizer.
#' 
#' @param x Either a matrix of data values, or a data frame, or a list of
#'     data frames of exactly three columns.
#'
#' @keywords graphs
#' @export
eafplot <- function(x, ...) UseMethod("eafplot")

#' @describeIn eafplot Main function
#' 
#' @param groups This may be used to plot profiles of different algorithms on the same plot.
#' 
#' @param subset (`integer()` | `NULL`)\cr A vector indicating which rows of the data should be used. If left to default `NULL` all data in the data frame are used.
#'  
#' @param sets ([numeric])\cr Vector indicating which set each point belongs to.
#' 
#' @param percentiles ([numeric])\cr Vector indicating which percentile should be plot. The
#'   default is to plot only the median attainment curve.
#' 
#' @param attsurfs   TODO
#' 
#' @param type (character(1))\cr string giving the type of plot desired.  The following values
#'   are possible, \samp{points} and \samp{area}.
#' 
#' @param xlab,ylab,xlim,ylim,log,col,lty,lwd,pch,cex.pch,las Graphical
#'   parameters, see [plot.default()].
#' 
#'@param legend.pos the position of the legend, see [legend()].  A value of `"none"` hides the legend.
#'
#'@param legend.txt a character or expression vector to appear in the
#'   legend. If `NULL`, appropriate labels will be generated.
#' 
#' @param extra.points A list of matrices or data.frames with
#'   two-columns. Each element of the list defines a set of points, or
#'   lines if one of the columns is `NA`.
#' 
#' @param extra.pch,extra.lwd,extra.lty,extra.col Control the graphical aspect
#'   of the points. See [points()] and [lines()].
#' 
#' @param extra.legend A character vector providing labels for the
#'   groups of points.
#' 
#' @template arg_maximise
#' 
#' @param xaxis.side On which side that xaxis is drawn. Valid values are
#'   "below" and "above". See [axis()].
#' 
#' @param yaxis.side On which side that yaxis is drawn. Valid values are "left"
#'   and "right". See [axis()].
#'   
#' @param axes A logical value indicating whether both axes should be drawn
#'   on the plot.
#'
#' @param sci.notation Generate prettier labels
#'
#' @param ... Other graphical parameters to [plot.default()].
#' 
#' @return No value is returned.
#' 
#' @seealso   [read_datasets()] [eafdiffplot()]
#'
#'@examples
#'data(gcp2x2)
#' tabucol <- subset(gcp2x2, alg != "TSinN1")
#' tabucol$alg <- tabucol$alg[drop=TRUE]
#' eafplot(time + best ~ run, data = tabucol, subset = tabucol$inst=="DSJC500.5")
#' 
#' \dontrun{ # These take time
#' eafplot(time + best ~ run | inst, groups=alg, data=gcp2x2)
#' eafplot(time + best ~ run | inst, groups=alg, data=gcp2x2,
#' 	percentiles=c(0,50,100), cex.axis = 1.2, lty = c(2,1,2), lwd = c(2,2,2),
#'      col = c("black","blue","grey50"))
#'
#' extdata_path <- system.file(package = "eaf", "extdata")
#' A1 <- read_datasets(file.path(extdata_path, "ALG_1_dat.xz"))
#' A2 <- read_datasets(file.path(extdata_path, "ALG_2_dat.xz"))
#' eafplot(A1, percentiles = 50, sci.notation = TRUE)
#' eafplot(list(A1 = A1, A2 = A2), percentiles = 50)
#' 
#' ## Save as a PDF file.
#' # dev.copy2pdf(file = "eaf.pdf", onefile = TRUE, width = 5, height = 4)
#' }
#' 
#' ## Using extra.points
#' \dontrun{
#' data(HybridGA)
#' data(SPEA2relativeVanzyl)
#' eafplot(SPEA2relativeVanzyl, percentiles = c(25, 50, 75), 
#'         xlab = expression(C[E]), ylab = "Total switches", xlim = c(320, 400),
#'         extra.points = HybridGA$vanzyl, extra.legend = "Hybrid GA")
#' 
#' data(SPEA2relativeRichmond)
#' eafplot (SPEA2relativeRichmond, percentiles = c(25, 50, 75),
#'          xlab = expression(C[E]), ylab = "Total switches",
#'          xlim = c(90, 140), ylim = c(0, 25),
#'          extra.points = HybridGA$richmond, extra.lty = "dashed",
#'          extra.legend = "Hybrid GA")
#' 
#' eafplot (SPEA2relativeRichmond, percentiles = c(25, 50, 75),
#'          xlab = expression(C[E]), ylab = "Total switches",
#'          xlim = c(90, 140), ylim = c(0, 25), type = "area",
#'          extra.points = HybridGA$richmond, extra.lty = "dashed",
#'          extra.legend = "Hybrid GA", legend.pos = "bottomright")
#' 
#' data(SPEA2minstoptimeRichmond)
#' SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
#' eafplot (SPEA2minstoptimeRichmond, xlab = expression(C[E]),
#'          ylab = "Minimum idle time (minutes)", maximise = c(FALSE, TRUE),
#'          las = 1, log = "y", main = "SPEA2 (Richmond)",
#'          legend.pos = "bottomright")
#' }
#' @export
#'@md
eafplot.default <-
  function (x, sets = NULL, groups = NULL,
            percentiles = c(0,50,100),
            attsurfs = NULL,
            xlab = NULL, ylab = NULL,
            xlim = NULL, ylim = NULL,
            log = "",
            type = "point",
            col = NULL,
            lty = c("dashed", "solid", "solid", "solid", "dashed"),
            lwd = 1.75,
            pch = NA,
            # FIXME: this allows partial matching if cex is passed, so passing cex has not effect. 
            cex.pch = par("cex"),
            las = par("las"),
            legend.pos = "topright",
            legend.txt = NULL,
            # FIXME: Can we get rid of the extra. stuff? Replace it with calling points after eafplot.default in examples and eafplot.pl.
            extra.points = NULL, extra.legend = NULL,
            extra.pch = 4:25,
            extra.lwd = 0.5,
            extra.lty = NA,
            extra.col = "black",
            maximise = c(FALSE, FALSE),
            xaxis.side = "below", yaxis.side = "left",
            axes = TRUE,
            sci.notation = FALSE,
            ... )
{
  type <- match.arg (type, c("point", "area"))
  maximise <- as.logical(maximise)
  xaxis.side <- match.arg (xaxis.side, c("below", "above"))
  yaxis.side <- match.arg (yaxis.side, c("left", "right"))
                      
  if (is.null(col)) {
    if (type == "point") {
      col <- c("black", "darkgrey", "black", "grey40", "darkgrey")
    } else {
      col <- c("grey", "black")
    }
  }

  if (is.null(xlab))
    xlab <- if(!is.null(colnames(x)[1])) colnames(x)[1] else "objective 1"
  if (is.null(ylab))
    ylab <- if(!is.null(colnames(x)[2])) colnames(x)[2] else "objective 2"
     
  if (!is.null (attsurfs)) {
    # Don't we need to apply maximise?
    attsurfs <- lapply(attsurfs, function(x) { as.matrix(x[, 1:2, drop=FALSE]) })
  } else {
    # FIXME: This is a bit of wasted effort. We should decide what is more
    # efficient, one large matrix or separate points and sets, then be
    # consistent everywhere.
    if (!is.null(sets)) x <- cbind(x, sets)
    x <- check.eaf.data(x)
    sets <- x[, 3L]
    x <- as.matrix(x[,1:2, drop=FALSE])
    x <- matrix.maximise(x, maximise)

    # Transform EAF matrix into attsurfs list.
    if (is.null(groups)) {
      attsurfs <- compute.eaf.as.list(cbind(x, sets), percentiles)
    } else {
      # FIXME: Is this equivalent to compute.eaf.as.list for each g?
      EAF <- eafs (x, sets, groups, percentiles)
      attsurfs <- list()
      groups <- factor(EAF$groups)
      for (g in levels(groups)) {
        tmp <- lapply(split.data.frame(EAF[groups == g,],
                                       as.factor(EAF[groups == g, 3])),
                      function(x) { as.matrix(x[, 1:2, drop=FALSE]) })
        attsurfs <- c(attsurfs, tmp)
      }
    }
    # FIXME: rm(EAF) to save memory ?
    attsurfs <- lapply(attsurfs, matrix.maximise, maximise = maximise)
  }

  # FIXME: We should take the range from the attsurfs to not make x mandatory.
  xlim <- get.xylim(xlim, maximise[1], data = x[,1])
  ylim <- get.xylim(ylim, maximise[2], data = x[,2])
  extreme <- get.extremes(xlim, ylim, maximise, log)

  # FIXME: Find a better way to handle different x-y scale.
  yscale <- 1
  ## if (ymaximise) {
  ##   #yscale <- 60
  ##   yreverse <- -1
  ##   attsurfs <- lapply (attsurfs, function (x)
  ##                       { x[,2] <- yreverse * x[,2] / yscale; x })
  ##   ylim <- yreverse * ylim / yscale
  ##   extreme[2] <- yreverse * extreme[2]
  ##   if (log == "y") extreme[2] <- 1
  ## }

  ## lab' A numerical vector of the form 'c(x, y, len)' which modifies
  ##     the default way that axes are annotated.  The values of 'x'
  ##     and 'y' give the (approximate) number of tickmarks on the x
  ##     and y axes and 'len' specifies the label length.  The default
  ##     is 'c(5, 5, 7)'.  Note that this only affects the way the
  ##     parameters 'xaxp' and 'yaxp' are set when the user coordinate
  ##     system is set up, and is not consulted when axes are drawn.
  ##     'len' _is unimplemented_ in R.
  args <- list(...)
  args <- args[names(args) %in% c("cex", "cex.lab", "cex.axis", "lab")]
  par_default <- list(cex = 1.0, cex.lab = 1.1, cex.axis = 1.0, lab = c(10,5,7))
  par_default <- modifyList(par_default, args)
  op <- par(par_default)
  on.exit(par(op))
  
  plot(xlim, ylim, type = "n", xlab = "", ylab = "",
       xlim = xlim, ylim = ylim, log = log, axes = FALSE, las = las,
       panel.first = ({
         if (axes) {
           plot.eaf.axis(xaxis.side, xlab, las = las, sci.notation = sci.notation)
           plot.eaf.axis(yaxis.side, ylab, las = las, sci.notation = sci.notation,
                         # FIXME: eafplot uses 2.2, why the difference?
                         line = 2.75)
         }
         
         # FIXME: Perhaps have a function plot.eaf.lines that computes
         # several percentiles for a single algorithm and then calls
         # points() or polygon() as appropriate to add attainment
         # surfaces to an existing plot. This way we can factor out
         # the code below and use it in plot.eaf and plot.eafdiff
         if (type == "area") {
           # FIXME (Proposition): allow the user to provide the palette colors?
           if (length(col) == 2) {
             colfunc <- colorRampPalette(col)
             col <- colfunc(length(attsurfs))
           } else if (length(col) != length(attsurfs)) {
             stop ("length(col) != 2, but with 'type=area', eafplot.default needs just two colors")
           }
           plot.eaf.full.area(attsurfs, extreme, maximise, col = col)
         } else {
           ## Recycle values
           lwd <- rep(lwd, length=length(attsurfs))
           lty <- rep(lty, length=length(attsurfs))
           col <- rep(col, length=length(attsurfs))
           if (!is.null(pch)) pch <- rep(pch, length=length(attsurfs))
           plot.eaf.full.lines(attsurfs, extreme, maximise,
                               col = col, lty = lty, lwd = lwd, pch = pch, cex = cex.pch)
         }
       }), ...)


  if (!is.null (extra.points)) {
    if (!is.list (extra.points[[1]])) {
      extra.name <- deparse(substitute(extra.points))
      extra.points <- list(extra.points)
      names(extra.points) <- extra.name
    }
    ## Recycle values
    extra.length <- length(extra.points)
    extra.lwd <- rep(extra.lwd, length=extra.length)
    extra.lty <- rep(extra.lty, length=extra.length)
    extra.col <- rep(extra.col, length=extra.length)
    extra.pch <- rep(extra.pch, length=extra.length)
    if (is.null(extra.legend)) {
      extra.legend <- names(extra.points)
      if (is.null(extra.legend))
        extra.legend <- paste0("extra.points ", 1:length(extra.points))
    }
    for (i in 1:length(extra.points)) {
      if (any(is.na(extra.points[[i]][,1]))) {
        if (is.na(extra.lty[i])) extra.lty <- "dashed"
        ## Extra points are given in the correct order so no reverse
        extra.points[[i]][,2] <- extra.points[[i]][,2] / yscale
        abline(h=extra.points[[i]][,2], lwd = extra.lwd[i], col = extra.col[i],
               lty = extra.lty[i])
        extra.pch[i] <- NA

      } else if (any(is.na(extra.points[[i]][,2]))) {
        if (is.na(extra.lty[i])) extra.lty <- "dashed"
        abline(v=extra.points[[i]][,1], lwd = extra.lwd[i], col = extra.col[i],
               lty = extra.lty[i])
        extra.pch[i] <- NA

      } else {
        ## Extra points are given in the correct order so no reverse
        extra.points[[i]][,2] <- extra.points[[i]][,2] / yscale
        if (!is.na(extra.pch[i])) 
          points (extra.points[[i]], type = "p", pch = extra.pch[i],
                  col = extra.col[i], cex = cex.pch)
        if (!is.na(extra.lty[i]))
          points (extra.points[[i]], type = "s", lty = extra.lty[i],
                  col = extra.col[i], lwd = extra.lwd[i])
      }
      lwd <- c(lwd, extra.lwd[i])
      lty <- c(lty, extra.lty[i])
      col <- c(col, extra.col[i])
      pch <- c(pch, extra.pch[i])
      if (is.null(extra.legend[i])) extra.legend[i]
    }
  }

  # Setup legend.
  if (is.null(legend.txt) && !is.null(percentiles)) {
    legend.txt <- paste0(percentiles, "%")
    legend.txt <- sub("^0%$", "best", legend.txt)
    legend.txt <- sub("^50%$", "median", legend.txt)
    legend.txt <- sub("^100%$", "worst", legend.txt)

    if (!is.null(groups)) {
      groups <- factor(groups)
      legend.txt <- as.vector(t(outer(levels(groups), legend.txt, paste)))
    }
  }
  legend.txt <- c(legend.txt, extra.legend)

  if (!is.null(legend.txt) && is.na(pmatch(legend.pos,"none"))) {
    if (type == "area") {
      legend(x = legend.pos, y = NULL,
             legend = legend.txt, fill = c(col, "#FFFFFF"),
             bg="white",bty="n", xjust=0, yjust=0, cex=0.9)
    } else {
      legend(legend.pos,
             legend = legend.txt, xjust=1, yjust=1, bty="n",
             lty = lty,  lwd = lwd, pch = pch, col = col, merge=T)
    }
  }

  box()
  invisible()
}

prettySciNotation <- function(x, digits = 1L)
{
  if (length(x) > 1L) {
    return(append(prettySciNotation(x[1]), prettySciNotation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  as.expression(substitute(base %*% 10^exponent,
                           list(base = base, exponent = exponent)))
}

axis.side <- function(side)
{
  if (!is.character(side)) return(side)
  return(switch(side,
                below = 1,
                left = 2,
                above = 3,
                right = 4))
}

plot.eaf.axis <- function(side, lab, las,
                          col = 'lightgray', lty = 'dotted', lwd = par("lwd"),
                          line = 2.1, sci.notation = FALSE)
{
  side <- axis.side(side)
  ## FIXME: Do we still need lwd=0.5, lty="26" to work-around for R bug?
  at <- axTicks(if (side %% 2 == 0) 2 else 1)
  labels <- if (sci.notation) prettySciNotation(at) else formatC(at, format = "g")
  ## if (log == "y") {
  ##   ## Custom log axis (like gnuplot but in R is hard)
  ##   max.pow <- 6
  ##   at <- c(1, 5, 10, 50, 100, 500, 1000, 1500, 10^c(4:max.pow))
  ##   labels <- c(1, 5, 10, 50, 100, 500, 1000, 1500,
  ##               parse(text = paste("10^", 4:max.pow, sep = "")))
  
  ##   #at <- c(60, 120, 180, 240, 300, 480, 600, 900, 1200, 1440)
  ##   #labels <- formatC(at,format="g")
  
  ##   ## Now do the minor ticks, at 1/10 of each power of 10 interval
  ##   ##at.minor <- 2:9 * rep(c(10^c(1:max.pow)) / 10, each = length(2:9))
  ##   at.minor <- 1:10 * rep(c(10^c(1:max.pow)) / 10, each = length(1:10))
  ##   axis (yaxis.side, at = at.minor, tcl = -0.25, labels = FALSE, las=las)
  ##   axis (yaxis.side, at = at.minor, labels = FALSE, tck=1,
  ##         col='lightgray', lty='dotted', lwd=par("lwd"))
  ## }
  
  ## tck=1 draws the horizontal grid lines (grid() is seriously broken).
  axis(side, at=at, labels=FALSE, tck = 1,
       col='lightgray', lty = 'dotted', lwd = par("lwd"))
  axis(side, at=at, labels=labels, las = las)
  mtext(lab, side, line = line, cex = par("cex") * par("cex.axis"),
        las = 0)
}

add.extremes <- function(x, extremes, maximise)
{
  best1 <- if (maximise[1]) max else min
  best2 <- if (maximise[2]) max else min
  return(rbind(c(best1(x[,1]), extremes[2]), x, c(extremes[1], best2(x[,2]))))
}

plot.eaf.full.lines <- function(attsurfs, extreme, maximise,
                                 col, lty, lwd, pch = NULL, cex = par("cex"))
{
  ## Recycle values
  lwd <- rep(lwd, length = length(attsurfs))
  lty <- rep(lty, length = length(attsurfs))
  col <- rep(col, length = length(attsurfs))
  if (!is.null(pch))
    pch <- rep(pch, length = length(attsurfs))

  attsurfs = lapply(attsurfs, add.extremes, extreme, maximise)
  for (k in seq_along(attsurfs)) {
    # FIXME: Is there a way to plot points and steps in one call?
    if (!is.null(pch))
      points(attsurfs[[k]], type = "p", col = col[k], pch = pch[k], cex = cex)
    points(attsurfs[[k]], type = "s", col = col[k], lty = lty[k], lwd = lwd[k])
  }
}

plot.eaf.full.area <- function(attsurfs, extreme, maximise, col)
{
  stopifnot(length(attsurfs) == length(col))
  for (i in seq_along(attsurfs)) {
    poli <- add.extremes(points.steps(attsurfs[[i]]), extreme, maximise)
    poli <- rbind(poli, extreme)
    polygon(poli[,1], poli[,2], border = NA, col = col[i])
  }
}

plot.eafdiff.side <- function (eafdiff, attsurfs = list(),
                               col = c("#FFFFFF", "#BFBFBF","#808080","#404040","#000000"),
                               side = stop("Argument 'side' is required"),
                               type = "point",
                               xlim = NULL, ylim = NULL, log = "",
                               las = par("las"),
                               full.eaf = FALSE,
                               title = "",
                               maximise = c(FALSE, FALSE),
                               xlab = "objective 1", ylab = "objective 2",
                               sci.notation = FALSE,
                               ...)
{
  type <- match.arg (type, c("point", "area"))
  maximise <- as.logical(maximise)
  side <- match.arg (side, c("left", "right"))
  xaxis.side <- if (side == "left") "below" else "above"
  yaxis.side <- if (side == "left") "left" else "right"

  # For !full.eaf && type == "area", str(eafdiff) is a polygon:
  ##  $  num [, 1:2]
  ##    - attr(*, "col")= num []

  # Colors are correct for !full.eaf && type == "area"
  if (full.eaf || type == "point") {
    # Why flooring and not ceiling? If a point has value 2.05, it should
    # be painted with color 2 rather than 3.
    # +1 because col[1] is white.
    eafdiff[,3] <- floor(eafdiff[,3]) + 1
    if (length(unique(eafdiff[,3])) > length(col)) {
      stop ("Too few colors: length(unique(eafdiff[,3])) > length(col)")
    }
  }

  # We do not paint with the same color as the background since this
  # will override the grid lines.
  ## FIXME: This should actually look at the bg color of the plot
  col[col == "#FFFFFF"] <- NA

  extreme <- get.extremes(xlim, ylim, maximise, log)
  yscale <- 1
  ## FIXME log == "y" and yscaling
  #    yscale <- 60
  ## if (yscale != 1) {
  ##   # This doesn't work with polygons.
  ##   stopifnot (full.eaf || type == "point")

  ##   eafdiff[,2] <- eafdiff[,2] / yscale
  ##   attsurfs <- lapply (attsurfs, function (x)
  ##                       { x[,2] <- x[,2] / yscale; x })
  ##   ylim <- ylim / yscale
  ##   if (log == "y") extreme[2] <- 1
  ## }
  
  plot(xlim, ylim, type = "n", xlab = "", ylab = "",
       xlim = xlim, ylim = ylim, log = log, axes = FALSE, las = las,
       panel.first = ({
         plot.eaf.axis (xaxis.side, xlab, las = las, sci.notation = sci.notation)
         plot.eaf.axis (yaxis.side, ylab, las = las, sci.notation = sci.notation,
                        line = 2.2)
                         
         if (nrow(eafdiff)) {
           if (type == "area") {
             if (full.eaf) {
               plot.eaf.full.area(split.data.frame(eafdiff[,1:2], eafdiff[,3]),
                                   extreme, maximise, col)
             } else {
               eafdiff[,1] <- rm.inf(eafdiff[,1], extreme[1])
               eafdiff[,2] <- rm.inf(eafdiff[,2], extreme[2])
               polycol <- attr(eafdiff, "col")
               #print(unique(polycol))
               #print(length(col))
               ## The maximum value should also be painted.
               polycol[polycol > length(col)] <- length(col)
               #print(eafdiff)
               #print(col[polycol])
               polygon(eafdiff[,1], eafdiff[,2], border = NA, col = col[polycol])
             }
           } else {
             ## The maximum value should also be painted.
             eafdiff[eafdiff[,3] > length(col), 3] <- length(col)
             eafdiff <- eafdiff[order(eafdiff[,3], decreasing = FALSE), , drop=FALSE]
             points(eafdiff[,1], eafdiff[,2], col = col[eafdiff[,3]], type = "p", pch=20)
           }
         }
       }), ...)

  lty <- c("solid", "dashed")
  lwd <- c(1)
  if (type == "area" && full.eaf) {
    col <- c("black", "black", "white")
  } else {
    col <- c("black")
  }

  plot.eaf.full.lines(attsurfs, extreme, maximise,
                      col = col, lty = lty, lwd = lwd)
  mtext(title, 1, line = 3.5, cex = par("cex.lab"), las = 0, font = 2)
  box()
}

#' Plot empirical attainment function differences 
#' 
#' Plot the differences between the empirical attainment functions of two
#' data sets as a two-panel plot, where the left side shows the values of
#' the left EAF minus the right EAF and the right side shows the
#' differences in the other direction.
#' 
#' @param data.left,data.right Data frames corresponding to the input data of
#'   left and right sides, respectively. Each data frame has at least three
#'   columns, the third one being the set of each point. See also
#'   [read_datasets()].
#' 
#' @param col A character vector of three colors for the magnitude of the
#'   differences of 0, 0.5, and 1. Intermediate colors are computed
#'   automatically given the value of `intervals`.
#' 
#' @param intervals (`integer(1)`|`character()`) \cr The
#'   absolute range of the differences \eqn{[0, 1]} is partitioned into the number
#'   of intervals provided. If an integer is provided, then labels for each
#'   interval are  computed automatically. If a character vector is
#'   provided, its length is taken as the number of intervals.
#' 
#' @param percentiles The percentiles of the EAF of each side that will be
#'   plotted as attainment surfaces. `NA` does not plot any. See
#'   [eafplot()].
#' 
#' @param full.eaf Whether to plot the EAF of each side instead of the
#'   differences between the EAFs.
#' 
#' @param type Whether the EAF differences are plotted as points
#'   (\samp{points}) or whether to color the areas that have at least a
#'   certain value (\samp{area}).
#' 
#'@param legend.pos The position of the legend. See [legend()].  A value of
#'   `"none"` hides the legend.
#' 
#'@param title.left,title.right Title for left and right panels, respectively.
#'  
#' @param xlim,ylim,cex,cex.lab,cex.axis Graphical parameters, see
#'   [plot.default()].
#' 
#' @template arg_maximise
#' 
#' @param grand.lines Whether to plot the grand-best and grand-worst
#'   attainment surfaces.
#'
#' @param sci.notation Generate prettier labels
#'
#' @param left.panel.last,right.panel.last An expression to be evaluated after
#'   plotting has taken place on each panel (left or right). This can be useful
#'   for adding points or text to either panel.  Note that this works by lazy
#'   evaluation: passing this argument from other `plot` methods may well
#'   not work since it may be evaluated too early.
#' 
#' @param ... Other graphical parameters are passed down to
#'   [plot.default()].
#' 
#' @details
#'   This function calculates the differences between the EAFs of two
#'   data sets, and plots on the left the differences in favour
#'   of the left data set, and on the right the differences in favour of
#'   the right data set. By default, it also plots the grand best and worst
#'   attainment surfaces, that is, the 0\% and 100\%-attainment surfaces
#'   over all data. This two surfaces delimit the area where differences
#'   may exist. In addition, it also plots the 50\%-attainment surface of
#'   each data set.
#'   
#'   With `type = "point"`, only the points where there is a change in
#'   the value of the EAF difference are plotted. This means that for areas
#'   where the EAF differences stays constant, the region will appear in
#'   white even if the value of the differences in that region is
#'   large. This explains "white holes" surrounded by black
#'   points.
#' 
#'   With `type = "area"`, the area where the EAF differences has a
#'   certain value is plotted.  The idea for the algorithm to compute the
#'   areas was provided by Carlos M. Fonseca.  The implementation uses R
#'   polygons, which some PDF viewers may have trouble rendering correctly
#'   (See
#'   \url{https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-are-there-unwanted-borders}). Plots (should) look correct when printed.
#' 
#'   Large differences that appear when using `type = "points"` may
#'   seem to disappear when using `type = "area"`. The explanation is
#'   the points size is independent of the axes range, therefore, the
#'   plotted points may seem to cover a much larger area than the actual
#'   number of points. On the other hand, the areas size is plotted with
#'   respect to the objective space, without any extra borders. If the
#'   range of an area becomes smaller than one-pixel, it won't be
#'   visible. As a consequence, zooming in or out certain regions of the plots
#'   does not change the apparent size of the points, whereas it affects
#'   considerably the apparent size of the areas.
#'   
#' 
#' @return No return value.
#' 
#' @seealso    [read_datasets()], [eafplot()]
#' 
#' @examples
#' extdata_dir <- system.file(package="eaf", "extdata") 
#' A1 <- read_datasets(file.path(extdata_dir, "ALG_1_dat.xz"))
#' A2 <- read_datasets(file.path(extdata_dir, "ALG_2_dat.xz"))
#' \donttest{# These take time
#'   eafdiffplot(A1, A2, full.eaf = TRUE)
#'   eafdiffplot(A1, A2, type = "area")
#'   eafdiffplot(A1, A2, type = "point", sci.notation = TRUE)
#' }
#' # A more complex example
#' a1 <- read_datasets(file.path(extdata_dir, "wrots_l100w10_dat"))
#' a2 <- read_datasets(file.path(extdata_dir, "wrots_l10w100_dat"))
#' DIFF <- eafdiffplot(a1, a2, col = c("white", "blue", "red"), intervals = 5,
#'                     type = "point",
#'                     title.left=expression("W-RoTS," ~ lambda==100 * "," ~ omega==10),
#'                     title.right=expression("W-RoTS," ~ lambda==10 * "," ~ omega==100),
#'                     right.panel.last={
#'                       abline(a = 0, b = 1, col = "red", lty = "dashed")})
#' DIFF$right[,3] <- -DIFF$right[,3]
#' 
#'  ## Save the values to a file.
#'  # write.table(rbind(DIFF$left,DIFF$right),
#'  #             file = "wrots_l100w10_dat-wrots_l10w100_dat-diff.txt",
#'  #             quote = FALSE, row.names = FALSE, col.names = FALSE)
#'  
#'@keywords graphs
#'@export
#'@md
eafdiffplot <-
  function(data.left, data.right,
           col = c("#FFFFFF", "#808080","#000000"),
           intervals = 5,
           percentiles = c(50),
           full.eaf = FALSE,
           type = "area",
           legend.pos = if (full.eaf) "bottomleft" else "topright",
           title.left = deparse(substitute(data.left)),
           title.right = deparse(substitute(data.right)),
           xlim = NULL, ylim = NULL,
           cex = par("cex"), cex.lab = par("cex.lab"), cex.axis = par("cex.axis"),
           maximise = c(FALSE, FALSE),
           grand.lines = TRUE,
           sci.notation = FALSE,
           left.panel.last = NULL,
           right.panel.last = NULL,
           ...)
{
  type <- match.arg (type, c("point", "area"))
  # FIXME: check that it is either an integer or a character vector.
  if (length(intervals) == 1) {
    intervals <- seq.intervals.labels(
      round(seq(0,1 , length.out = 1 + intervals), 4), digits = 1)
  }
  if (length(col) != 3) {
    stop ("'col' must provide three colors (minimum, medium maximum)")
  }
  col <- colorRampPalette(col)(length(intervals))
  title.left <- title.left
  title.right <- title.right

  maximise <- as.logical(maximise)
  if (length(maximise) == 1) {
    maximise <- rep_len(maximise, 2)
  } else if (length(maximise) != 2) {
    stop("length of maximise must be either 1 or 2")
  }

  data.left <- check.eaf.data(data.left)
  data.left[,1:2] <- matrix.maximise(data.left[,1:2, drop=FALSE], maximise)
  data.right <- check.eaf.data(data.right)
  data.right[,1:2] <- matrix.maximise(data.right[,1:2, drop=FALSE], maximise)

  attsurfs.left <- attsurfs.right <- list()
  if (!any(is.na(percentiles))) {
    attsurfs.left <- compute.eaf.as.list (data.left, percentiles)
    attsurfs.left <- lapply(attsurfs.left, matrix.maximise, maximise = maximise)
    attsurfs.right <- compute.eaf.as.list (data.right, percentiles)
    attsurfs.right <- lapply(attsurfs.right, matrix.maximise, maximise = maximise)
  }

  # FIXME: We do not need this for the full EAF.
  # Merge the data
  data.combined <- rbind_datasets(data.left, data.right)

  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  on.exit(par(def.par))

  if (full.eaf) {
    if (type == "area") {
      lower.boundaries <- 0:(length(intervals)-1) * 100 / length(intervals)
      diff_left <- compute.eaf (data.left, percentiles = lower.boundaries)
      diff_right <- compute.eaf (data.right, percentiles = lower.boundaries)
    } else if (type == "point") {
      diff_left <- compute.eaf (data.left)
      diff_right <- compute.eaf (data.right)
      # Since plot.eafdiff.side uses floor to calculate the color, and
      # we want color[100] == color[99].
      diff_left[diff_left[,3] == 100, 3] <- 99
      diff_right[diff_right[,3] == 100, 3] <- 99
    }
    # Convert percentile into color index
    diff_left[,3] <- diff_left[,3] * length(intervals) / 100
    diff_right[,3] <- diff_right[,3] * length(intervals) / 100
    #remove(data.left,data.right,data.combined) # Free memory?
  } else {
    if (type == "area") {
      DIFF <- compute.eafdiff.polygon (data.combined, intervals = length(intervals))
    } else if (type == "point") {
      #remove(data.left,data.right) # Free memory?
      DIFF <- compute.eafdiff (data.combined, intervals = length(intervals))
      #remove(data.combined) # Free memory?
    }
    diff_left <- DIFF$left
    diff_right <- DIFF$right
  }
    
  # FIXME: This can be avoided and just taken from the full EAF.
  grand.attsurf <- compute.eaf.as.list (data.combined, c(0, 100))
  grand.best <- grand.attsurf[["0"]]
  grand.worst <- grand.attsurf[["100"]]

  xlim <- get.xylim(xlim, maximise[1],
                    data = c(grand.best[,1], grand.worst[,1],
                             range.finite(diff_left[,1]), range.finite(diff_right[,1])))
  ylim <- get.xylim(ylim, maximise[2],
                    data = c(grand.best[,2], grand.worst[,2],
                             range.finite(diff_left[,2]), range.finite(diff_right[,2])))

  grand.best <- matrix.maximise(grand.best, maximise)
  grand.worst <- matrix.maximise(grand.worst, maximise)
  diff_left[,1:2] <- matrix.maximise(diff_left[,1:2, drop=FALSE], maximise)
  diff_right[,1:2] <- matrix.maximise(diff_right[,1:2, drop=FALSE], maximise)
  
  # FIXME: This does not generate empty space between the two plots, but the
  # plots are not squared.
  layout(matrix(1:2, ncol=2, byrow=TRUE), respect=TRUE)
  bottommar <- 5
  topmar   <- 4
  leftmar  <- 4
  rightmar <- 4
  
  # FIXME: This generates empty spaces between the two plots. How to ensure
  # that the side-by-side plots are kept together?
  ## layout(matrix(1:2, ncol = 2))
  ## par (pty = 's') # Force it to be square
  ## bottommar <- 5
  ## topmar   <- 4
  ## leftmar  <- 4
  ## rightmar <- 4

  # cex.axis is multiplied by cex, but cex.lab is not.
  par(cex = cex, cex.lab = cex.lab, cex.axis = cex.axis
    , mar = c(bottommar, leftmar, topmar, 0)
    , lab = c(10,5,7)
    , las = 0
      )

  if (grand.lines) {
    attsurfs <- c(list(grand.best), attsurfs.left, list(grand.worst))
  } else {
    attsurfs <- attsurfs.left
  }
    
  plot.eafdiff.side (diff_left,
                     attsurfs = attsurfs,
                     col = col,
                     type = type, full.eaf = full.eaf,
                     title = title.left,
                     xlim = xlim, ylim = ylim,
                     side = "left", maximise = maximise,
                     sci.notation = sci.notation, ...)

  if (is.na(pmatch(legend.pos,"none"))) {
    #nchar(legend.pos) > 0 && !(legend.pos %in% c("no", "none"))) {
    legend(x = legend.pos, y = NULL,
           rev(intervals), rev(col),
           bg = "white", bty = "n", xjust=0, yjust=0, cex=0.9)
  }
  left.panel.last
  
  par(mar = c(bottommar, 0, topmar, rightmar))

  if (grand.lines) {
    attsurfs <- c(list(grand.best), attsurfs.right, list(grand.worst))
  } else {
    attsurfs <- attsurfs.right
  }
  plot.eafdiff.side (diff_right,
                     attsurfs = attsurfs,
                     col = col,
                     type = type, full.eaf = full.eaf,
                     title = title.right,
                     xlim = xlim, ylim = ylim,
                     side = "right", maximise = maximise,
                     sci.notation = sci.notation, ...)
  right.panel.last
  invisible(list(left=diff_left, right=diff_right))
}

# Create labels:
# eaf:::seq.intervals.labels(seq(0,1, length.out=5), digits = 1)
# "[0.0, 0.2)" "[0.2, 0.4)" "[0.4, 0.6)" "[0.6, 0.8)" "[0.8, 1.0]"
# FIXME: Add examples and tests
seq.intervals.labels <- function(s, first.open = FALSE, last.open = FALSE,
                                 digits = NULL)
{
  # FIXME:  This should use:
  # levels(cut(0, s, dig.lab=digits, include.lowest=TRUE, right=FALSE))
  s <- formatC(s, digits = digits, format = if (is.null(digits)) "g" else "f")
  if (length(s) < 2) stop ("sequence must have at least 2 values")
  intervals <- paste0("[", s[-length(s)], ", ", s[-1], ")")
  if (first.open)
    substr(intervals[1], 0, 1) <- "("
  if (!last.open) {
    len <- nchar(intervals[length(intervals)])
    substr(intervals[length(intervals)], len, len+1) <- "]"
  }
  return(intervals)
}



#' @describeIn eafplot Formula interface
#'
#'@param formula A formula of the type: \code{time + cost ~ run | instance}
#'     will draw \code{time} on the x-axis and \code{cost} on the y-axis. If \code{instance} is
#'     present the plot is conditional to the instances.
#' 
#'@param data Dataframe containing the fields mentioned in the formula and in groups.
#'@export
eafplot.formula <- function(formula, data, groups = NULL, subset = NULL, ...)
{
  ## formula of type time+cost~run|inst, groups=alg
  if (missing(formula))
    stop("formula missing")
 
  if ((length(formula) != 3L) || (length(formula[[2L]]) != 3L))
    stop("incorrect specification for 'formula'")

  mf <- modeltools::ModelEnvFormula(formula = formula, data = data,
                                    subset = subset, designMatrix = FALSE,
                                    responseMatrix = FALSE, ...)

  ### extract data from the ModelEnv object
  points <- mf@get("response")
  sets <- mf@get("input")
  cond <- NULL
  if (length(mf@formula) == 3L)
    cond <- as.list(mf@get("blocks"))

  groups <- eval(substitute(groups), data, environment(formula))

  if (!is.null(groups) && !is.factor(groups))
    stop("groups must be a factor")
  
  if (length(cond) == 0)
    {
      strip <- FALSE
      cond <- list(gl(1, length(points)))
    }

  condlevels <- lapply(cond, levels)
  cond.max.level <- unlist(lapply(cond, nlevels))

  npackets <- prod(cond.max.level)

  panel.args <- vector(mode = "list", length = npackets)

  packet.sizes <- numeric(npackets)
  if (npackets > 1)
    {
      dim(packet.sizes) <- sapply(condlevels, length)
      dimnames(packet.sizes) <- lapply(condlevels, as.character)
    }

  cond.current.level <- rep(1, length(cond))

  for (packet.number in seq_len(npackets)) {
    id <- .compute.packet(cond, cond.current.level)
    packet.sizes[packet.number] <- sum(id)

    panel.args[[packet.number]] <-
      list(points = as.matrix(points[id,]), sets = as.numeric(sets[id,]),
           groups=groups[id])
    ## MARCO: I do not think we need to care about subscripts... or do we?
    #if (subscripts)
    #   panel.args[[packet.number]]$subscripts <-
    #                   subscr[id]
    cond.current.level <- .cupdate(cond.current.level, cond.max.level)
  }
  op <- par(no.readonly = TRUE)  # save default, for resetting...
  on.exit(par(op))
  ## FIXME: I don't think this is doing the right thing.
  par(mfrow = .check.layout(NULL,cond.max.level)[2:3])
  for (i in seq_len(length(panel.args))) {
    eafplot.default(panel.args[[i]]$points,
                    panel.args[[i]]$sets,
                    panel.args[[i]]$groups,
                    ...)
  }
  invisible()
}


#' @describeIn eafplot List interface for lists of data.frames or matrices
#' 
#'@export 
eafplot.list <- function(x, ...)
{
  if (!is.list(x))
    stop("'x' must be a list of data.frames or matrices with exactly three columns")

  groups <- if (!is.null(names(x))) names(x) else 1:length(x)

  check.elem <- function(elem) {
    elem <- check.eaf.data(elem)
    if (ncol(elem) != 3L)
      stop("Each element of the list have exactly three columns. If you have grouping and conditioning variables, please consider using this format: 'eafplot(formula, data, ...)'")
    return(elem)
  }
  x <- lapply(x, check.elem)
  groups <- rep(groups, sapply(x, nrow))
  x <- do.call(rbind, x)
  
  eafplot(as.matrix(x[,c(1,2)]),
          sets = as.numeric(as.factor(x[, 3])),
          groups = groups, ...)
}

### Local Variables:
### ess-indent-level: 2
### ess-continued-statement-offset: 2
### ess-brace-offset: 0
### ess-expression-offset: 4
### ess-else-offset: 0
### ess-brace-imaginary-offset: 0
### ess-continued-brace-offset: 0
### ess-arg-function-offset: 2
### ess-close-brace-offset: 0
### indent-tabs-mode: nil
### ess-fancy-comments: nil
### End:
