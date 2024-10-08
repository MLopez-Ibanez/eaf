% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eafplot.R
\name{eafplot}
\alias{eafplot}
\alias{eafplot.default}
\alias{eafplot.formula}
\alias{eafplot.list}
\title{Plot the Empirical Attainment Function for two objectives}
\usage{
eafplot(x, ...)

\method{eafplot}{default}(
  x,
  sets = NULL,
  groups = NULL,
  percentiles = c(0, 50, 100),
  attsurfs = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  log = "",
  type = "point",
  col = NULL,
  lty = c("dashed", "solid", "solid", "solid", "dashed"),
  lwd = 1.75,
  pch = NA,
  cex.pch = par("cex"),
  las = par("las"),
  legend.pos = "topright",
  legend.txt = NULL,
  extra.points = NULL,
  extra.legend = NULL,
  extra.pch = 4:25,
  extra.lwd = 0.5,
  extra.lty = NA,
  extra.col = "black",
  maximise = c(FALSE, FALSE),
  xaxis.side = "below",
  yaxis.side = "left",
  axes = TRUE,
  sci.notation = FALSE,
  ...
)

\method{eafplot}{formula}(formula, data, groups = NULL, subset = NULL, ...)

\method{eafplot}{list}(x, ...)
}
\arguments{
\item{x}{Either a matrix of data values, or a data frame, or a list of
data frames of exactly three columns.}

\item{...}{Other graphical parameters to \code{\link[=plot.default]{plot.default()}}.}

\item{sets}{(\link{numeric})\cr Vector indicating which set each point belongs to.}

\item{groups}{This may be used to plot profiles of different algorithms on the same plot.}

\item{percentiles}{(\code{numeric()}) Vector indicating which percentile should be plot. The
default is to plot only the median attainment curve.}

\item{attsurfs}{TODO}

\item{xlab, ylab, xlim, ylim, log, col, lty, lwd, pch, cex.pch, las}{Graphical
parameters, see \code{\link[=plot.default]{plot.default()}}.}

\item{type}{(\code{character(1)})\cr string giving the type of plot desired.  The following values
are possible, \samp{points} and \samp{area}.}

\item{legend.pos}{the position of the legend, see \code{\link[=legend]{legend()}}.  A value of \code{"none"} hides the legend.}

\item{legend.txt}{a character or expression vector to appear in the
legend. If \code{NULL}, appropriate labels will be generated.}

\item{extra.points}{A list of matrices or data.frames with
two-columns. Each element of the list defines a set of points, or
lines if one of the columns is \code{NA}.}

\item{extra.legend}{A character vector providing labels for the
groups of points.}

\item{extra.pch, extra.lwd, extra.lty, extra.col}{Control the graphical aspect
of the points. See \code{\link[=points]{points()}} and \code{\link[=lines]{lines()}}.}

\item{maximise}{(\code{logical()} | \code{logical(1)}) \cr Whether the objectives must be
maximised instead of minimised. Either a single logical value that applies
to all objectives or a vector of logical values, with one value per
objective.}

\item{xaxis.side}{On which side that x-axis is drawn. Valid values are
\code{"below"} and \code{"above"}. See \code{\link[=axis]{axis()}}.}

\item{yaxis.side}{On which side that y-axis is drawn. Valid values are \code{"left"}
and \code{"right"}. See \code{\link[=axis]{axis()}}.}

\item{axes}{A logical value indicating whether both axes should be drawn
on the plot.}

\item{sci.notation}{Generate prettier labels}

\item{formula}{A formula of the type: \code{time + cost ~ run | instance}
will draw \code{time} on the x-axis and \code{cost} on the y-axis. If \code{instance} is
present the plot is conditional to the instances.}

\item{data}{Dataframe containing the fields mentioned in the formula and in groups.}

\item{subset}{(\code{integer()} | \code{NULL})\cr A vector indicating which rows of the data should be used. If left to default \code{NULL} all data in the data frame are used.}
}
\value{
Return (invisibly) the attainment surfaces computed.
}
\description{
Computes and plots the Empirical Attainment Function, either as
attainment surfaces for certain percentiles or as points.
}
\details{
This function can be used to plot random sets of points like those obtained
by different runs of biobjective stochastic optimisation algorithms.  An EAF
curve represents the boundary separating points that are known to be
attainable (that is, dominated in Pareto sense) in at least a fraction
(quantile) of the runs from those that are not. The median EAF represents
the curve where the fraction of attainable points is 50\%.  In single
objective optimisation the function can be used to plot the profile of
solution quality over time of a collection of runs of a stochastic optimizer.
}
\section{Methods (by class)}{
\itemize{
\item \code{eafplot(default)}: Main function

\item \code{eafplot(formula)}: Formula interface

\item \code{eafplot(list)}: List interface for lists of data.frames or matrices

}}
\examples{
data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop=TRUE]
eafplot(time + best ~ run, data = tabucol, subset = tabucol$inst=="DSJC500.5")

\donttest{# These take time
eafplot(time + best ~ run | inst, groups=alg, data=gcp2x2)
eafplot(time + best ~ run | inst, groups=alg, data=gcp2x2,
	percentiles=c(0,50,100), cex.axis = 0.8, lty = c(2,1,2), lwd = c(2,2,2),
     col = c("black","blue","grey50"))

extdata_path <- system.file(package = "eaf", "extdata")
A1 <- read_datasets(file.path(extdata_path, "ALG_1_dat.xz"))
A2 <- read_datasets(file.path(extdata_path, "ALG_2_dat.xz"))
eafplot(A1, percentiles = 50, sci.notation = TRUE, cex.axis=0.6)
# The attainment surfaces are returned invisibly.
attsurfs <- eafplot(list(A1 = A1, A2 = A2), percentiles = 50)
str(attsurfs)

## Save as a PDF file.
# dev.copy2pdf(file = "eaf.pdf", onefile = TRUE, width = 5, height = 4)
}

## Using extra.points
\donttest{
data(HybridGA)
data(SPEA2relativeVanzyl)
eafplot(SPEA2relativeVanzyl, percentiles = c(25, 50, 75), 
        xlab = expression(C[E]), ylab = "Total switches", xlim = c(320, 400),
        extra.points = HybridGA$vanzyl, extra.legend = "Hybrid GA")

data(SPEA2relativeRichmond)
eafplot (SPEA2relativeRichmond, percentiles = c(25, 50, 75),
         xlab = expression(C[E]), ylab = "Total switches",
         xlim = c(90, 140), ylim = c(0, 25),
         extra.points = HybridGA$richmond, extra.lty = "dashed",
         extra.legend = "Hybrid GA")

eafplot (SPEA2relativeRichmond, percentiles = c(25, 50, 75),
         xlab = expression(C[E]), ylab = "Total switches",
         xlim = c(90, 140), ylim = c(0, 25), type = "area",
         extra.points = HybridGA$richmond, extra.lty = "dashed",
         extra.legend = "Hybrid GA", legend.pos = "bottomright")

data(SPEA2minstoptimeRichmond)
SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
eafplot (SPEA2minstoptimeRichmond, xlab = expression(C[E]),
         ylab = "Minimum idle time (minutes)", maximise = c(FALSE, TRUE),
         las = 1, log = "y", main = "SPEA2 (Richmond)",
         legend.pos = "bottomright")
}
}
\seealso{
\code{\link[=eafdiffplot]{eafdiffplot()}} \code{\link[=pdf_crop]{pdf_crop()}}
}
\concept{visualisation}
