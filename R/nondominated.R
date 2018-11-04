check_dataset <- function(x)
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


#' Identify nondominated points with \code{is_nondominated} and remove dominated
#' ones with \code{filter_dominated}.
#' 
#' @title Identify, remove and rank dominated points according to Pareto optimality
#' @rdname nondominated
#'
#' @param data Either a matrix or a data frame of numerical values, where
#'   each row gives the coordinates of a point.
#'
#' @param maximise Whether the objectives must be maximised instead of
#'   minimised. Either a single logical value that applies to all objectives or
#'   a vector of logical values, with one value per objective.
#' 
#' @param keep_weakly If \code{FALSE}, return \code{FALSE} for any duplicates
#'   of nondominated points.
#' 
#' @return \code{is_nondominated} returns a logical vector of the same length
#'   as the number of rows of \code{data}, where \code{TRUE} means that the
#'   point is not dominated by any other point.
#'
#' @author Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#'
#' @examples
#' path_A1 <- file.path(system.file(package="eaf"),"extdata","ALG_1_dat")
#' set <- read_datasets(path_A1)[,1:2]
#'
#' is_nondom <- is_nondominated(set)
#' cat("There are ", sum(is_nondom), " nondominated points\n")
#'
#' plot(set, col = "blue", type = "p", pch = 20)
#' ndset <- filter_dominated(set)
#' points(ndset[order(ndset[,1]),], col = "red", pch = 21)
#' 
#' @export
is_nondominated <- function(data, maximise = FALSE, keep_weakly = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data)
  npoints <- nrow(data)
  maximise <- as.logical(rep_len(maximise, nobjs))

  return(.Call("is_nondominated_C",
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints),
               maximise,
               as.logical(keep_weakly)))
}

#' @rdname nondominated
#' @export
#' @return \code{filter_dominated} returns a matrix or data.frame with only mutually nondominated points.
#'
filter_dominated <- function(data, maximise = FALSE, keep_weakly = FALSE)
{
  return(data[is_nondominated(data, maximise = maximise, keep_weakly = keep_weakly),
            , drop = FALSE])
}

#' \code{pareto_rank} ranks points according to Pareto-optimality, which is also called
#' nondominated sorting (Deb et al., 2002).
#' 
#' @rdname nondominated
#' @export
#' @return \code{pareto_rank} returns an integer vector of the same length as
#'   the number of rows of \code{data}, where each value gives the rank of each
#'   point.
#'
#' @details \code{pareto_rank} is meant to be used like \code{rank()}, but it
#'   assigns ranks according to Pareto dominance. Duplicated points are kept on
#'   the same front. When \code{ncol(data) == 2}, the code uses the \eqn{O(n
#'   \log n)} algorithm by Jensen (2003).
#' 
#' @references
#'
#' Deb, K., S. Agrawal, A. Pratap, and T. Meyarivan. A fast elitist non-dominated
#' sorting genetic algorithm for multi-objective optimization: NSGA-II.
#' IEEE Transactions on Evolutionary Computation, 6(2): 182-197, 2002.
#' 
#' M. T. Jensen. Reducing the run-time complexity of multiobjective EAs: The
#' NSGA-II and other algorithms. IEEE Transactions on Evolutionary Computation,
#' 7(5):503–515, 2003.
#' 
#' @examples
#' ranks <- pareto_rank(set)
#' colors <- colorRampPalette(c("red","yellow","springgreen","royalblue"))(max(ranks))
#' plot(set, col = colors[ranks], type = "p", pch = 20)
#'
pareto_rank <- function(data, maximise = FALSE)
{
  data <- check_dataset(data)
  nobjs <- ncol(data)
  npoints <- nrow(data)
  maximise <- as.logical(rep_len(maximise, nobjs))
  data <- matrix.maximise(data, maximise)
  return(.Call("pareto_ranking_C",
               as.double(t(data)),
               as.integer(nobjs),
               as.integer(npoints)))
}
