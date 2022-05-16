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
#' the curve where the fraction of attainable points is 50%.  In single
#' objective optimization the function can be used to plot the profile of
#' solution quality over time of a collection of runs of a stochastic optimizer.
#' 
#' @param x Either a matrix of data values, or a data frame, or a list of
#'     data frames of exactly three columns.
#'
#' @concept eafviz
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
#' @param percentiles (`numeric()`) Vector indicating which percentile should be plot. The
#'   default is to plot only the median attainment curve.
#' 
#' @param attsurfs   TODO
#' 
#' @param type (`character(1)`)\cr string giving the type of plot desired.  The following values
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
#' @return Return (invisibly) the attainment surfaces computed.
#' 
#' @seealso   [read_datasets()] [eafdiffplot()] [pdf_crop()]
#'
#'@examples
#' data(gcp2x2)
#' tabucol <- subset(gcp2x2, alg != "TSinN1")
#' tabucol$alg <- tabucol$alg[drop=TRUE]
#' eafplot(time + best ~ run, data = tabucol, subset = tabucol$inst=="DSJC500.5")
#' 
#' \dontrun{# These take time
#' eafplot(time + best ~ run | inst, groups=alg, data=gcp2x2)
#' eafplot(time + best ~ run | inst, groups=alg, data=gcp2x2,
#' 	percentiles=c(0,50,100), cex.axis = 0.8, lty = c(2,1,2), lwd = c(2,2,2),
#'      col = c("black","blue","grey50"))
#'
#' extdata_path <- system.file(package = "eaf", "extdata")
#' A1 <- read_datasets(file.path(extdata_path, "ALG_1_dat.xz"))
#' A2 <- read_datasets(file.path(extdata_path, "ALG_2_dat.xz"))
#' eafplot(A1, percentiles = 50, sci.notation = TRUE, cex.axis=0.6)
#' # The attainment surfaces are returned invisibly.
#' attsurfs <- eafplot(list(A1 = A1, A2 = A2), percentiles = 50)
#' str(attsurfs)
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
#' @concept eafviz
#' @export
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
  xaxis.side <- match.arg(xaxis.side, c("below", "above"))
  yaxis.side <- match.arg(yaxis.side, c("left", "right"))
                      
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
      EAF <- eafs(x, sets, groups, percentiles)
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
  invisible(attsurfs)
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
                               col,
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
    # FIXME: This is wrong, we should color (0.0, 1] with col[1], then (1, 2]
    # with col[1], etc, so that we never color the value 0.0 but we always
    # color the maximum value color without having to force it.
    
    # Why flooring and not ceiling? If a point has value 2.05, it should
    # be painted with color 2 rather than 3.
    # +1 because col[1] is white ([0,1)).
    eafdiff[,3] <- floor(eafdiff[,3]) + 1
    if (length(unique(eafdiff[,3])) > length(col)) {
      stop ("Too few colors: length(unique(eafdiff[,3])) > length(col)")
    }
  }

  # We do not paint with the same color as the background since this
  # will override the grid lines.
  col[col %in% c("white", "#FFFFFF")] <- "transparent"

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
               # FIXME: How can this happen???
               polycol[polycol > length(col)] <- length(col)
               ### For debugging:
               ## poly_id <- head(1 + cumsum(is.na(eafdiff[,1])),n=-1)
               ## for(i in which(polycol == 10)) {
               ##   c_eafdiff <- eafdiff[i == poly_id, ]
               ##   polygon(c_eafdiff[,1], c_eafdiff[,2], border = FALSE, col = col[polycol[i]])
               ## }
               #print(eafdiff)
               #print(col[polycol])
               # FIXME: This reduces the number of artifacts but increases the memory consumption of embedFonts(filename) until it crashes.
               #polygon(eafdiff[,1], eafdiff[,2], border = col[polycol], lwd=0.1, col = col[polycol])
               polygon(eafdiff[,1], eafdiff[,2], border = col[polycol], col = col[polycol])
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
#' Plot the differences between the empirical attainment functions (EAFs) of two
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
#'   automatically given the value of `intervals`. Alternatively, a function
#'   such as [viridisLite::viridis()] that generates a colormap given an integer
#'   argument.
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
#'   attainment surfaces, that is, the 0%- and 100%-attainment surfaces
#'   over all data. These two surfaces delimit the area where differences
#'   may exist. In addition, it also plots the 50%-attainment surface of
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
#'   Large differences that appear when using `type = "point"` may
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
#' @return Returns a representation of the EAF differences (invisibly).
#' 
#' @seealso    [read_datasets()] [eafplot()] [pdf_crop()]
#' 
#' @examples
#' ## NOTE: The plots in the website look squashed because of how pkgdown
#' ## generates them. They should look fine when you generate them yourself.
#' extdata_dir <- system.file(package="eaf", "extdata") 
#' A1 <- read_datasets(file.path(extdata_dir, "ALG_1_dat.xz"))
#' A2 <- read_datasets(file.path(extdata_dir, "ALG_2_dat.xz"))
#' \donttest{# These take time
#' eafdiffplot(A1, A2, full.eaf = TRUE)
#' if (requireNamespace("viridisLite", quietly=TRUE)) {
#'   viridis_r <- function(n) viridisLite::viridis(n, direction=-1)
#'   eafdiffplot(A1, A2, type = "area", col = viridis_r)
#' } else {
#'   eafdiffplot(A1, A2, type = "area")
#' }
#' A1 <- read_datasets(file.path(extdata_dir, "wrots_l100w10_dat"))
#' A2 <- read_datasets(file.path(extdata_dir, "wrots_l10w100_dat"))
#' eafdiffplot(A1, A2, type = "point", sci.notation = TRUE, cex.axis=0.6)
#' }
#' # A more complex example
#' DIFF <- eafdiffplot(A1, A2, col = c("white", "blue", "red"), intervals = 5,
#'                     type = "point",
#'                     title.left=expression("W-RoTS," ~ lambda==100 * "," ~ omega==10),
#'                     title.right=expression("W-RoTS," ~ lambda==10 * "," ~ omega==100),
#'                     right.panel.last={
#'                       abline(a = 0, b = 1, col = "red", lty = "dashed")})
#' DIFF$right[,3] <- -DIFF$right[,3]
#' 
#' ## Save the values to a file.
#' # write.table(rbind(DIFF$left,DIFF$right),
#' #             file = "wrots_l100w10_dat-wrots_l10w100_dat-diff.txt",
#' #             quote = FALSE, row.names = FALSE, col.names = FALSE)
#'
#' @concept eafviz
#'@export
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
  if (is.function(col)) { # It is a color-map, like viridis()
    col <- col(length(intervals))
  } else {
    if (length(col) != 3) {
      stop ("'col' is either three colors (minimum, medium maximum) or a function that produces a colormap")
    }
    col <- colorRampPalette(col)(length(intervals))
  }
  # FIXME: The lowest color must be white (it should be the background).
  col[1] <- "white"
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

  if (is.na(pmatch(legend.pos,"none"))){ 
    #nchar(legend.pos) > 0 && !(legend.pos %in% c("no", "none"))) {
    legend(x = legend.pos, y = NULL,
           rev(intervals), rev(col),
           bg = "white", bty = "n", xjust=0, yjust=0, cex=0.85)
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
#'@concept eafviz
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
  # save default, for resetting...
  ## FIXME: I don't think this is doing the right thing.
  op <- par(mfrow = .check.layout(NULL,cond.max.level)[2:3],
            mar = c(4,4,1,1)+0.1)
  on.exit(par(op))
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
#'@concept eafviz
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
  
  eafplot(as.matrix(x[,c(1L,2L)]),
          sets = as.numeric(as.factor(x[, 3L])),
          groups = groups, ...)
}

