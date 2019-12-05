###############################################################################
#
#                          Copyright (c) 2011-2018
#         Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>
#             Marco Chiarandini <marco@imada.sdu.dk>
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
# manuel.lopez-ibanez@manchester.ac.uk with citations of papers referencing this
# work so I can mention them to my funding agent and tenure committee.
#
################################################################################
#
# TODO:
#
#  * Follow this style for coding:
#    http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#
################################################################################

#' Plot the Empirical Attainment Function for two objectives generic
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
  x <- sapply(x, check.elem)  
  groups <- rep(groups, sapply(x, nrow))
  x <- do.call(rbind, x)
  
  eafplot(as.matrix(x[,c(1,2)]),
          sets = as.numeric(as.factor(x[, 3])),
          groups = groups, ...)
}

### Local Variables:
### End:

