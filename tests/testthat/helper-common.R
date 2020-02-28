library(eaf)
# This file is loaded automatically by testthat (supposedly)
extdata.path <- function(file)
  return(file.path(system.file(package = "eaf"), "extdata", file))

read_extdata <- function(file) read_datasets(extdata.path(file))

## help_plot <- function(A, B)
## {
##   library(ggplot2)
##   x <- rbind(cbind(as.data.frame(A), algo="A"),cbind(as.data.frame(B), algo="B"))
##   DIFF <- eafdiff(A,B, rectangles=FALSE)
##   rectangles <- eafdiff(A,B, rectangles=TRUE)
##   colnames(DIFF) <- c("x","y", "color")
##   p <- ggplot(data=x) + geom_point(aes(x=V1,y=V2, color=algo, shape=as.factor(set))) + scale_shape_manual(values=c(0, 9, 1:8))
##   p <- p + geom_rect(data=as.data.frame(rectangles), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=as.factor(diff)), color="black", alpha=0.5)
##   p <- p + geom_text(data=as.data.frame(DIFF), aes(x=x,y=y,label=color), nudge_x = 0.05, nudge_y = 0.05)
##   print(p)
## }

