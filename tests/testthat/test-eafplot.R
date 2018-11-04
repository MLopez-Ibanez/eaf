library(eaf)
context("eafplot")

test_that("eafplot", {
skip_on_cran()
pdf(file = "eafplot.pdf", title = "eafplot.pdf", width = 9,  height = 6)
## FIXME: Add main=invokation
## FIXME: We need smaller data!
eaftest <- function(a, b, maximise = c(FALSE, FALSE)) {
  A1 <- read_datasets(file.path(system.file(package="eaf"), "extdata", a))
  A2 <- read_datasets(file.path(system.file(package="eaf"), "extdata", b))
  if (!any(maximise)) {
    # FIXME: Colors are wrong
    eafplot(A1, type = "area", legend.pos = "bottomleft")
    eafplot(A1, type = "point")
    eafplot(A1, type = "point", pch = 20)
    # FIXME: This doesn't plot anything useful.
    eafplot(list(A1 = A1, A2 = A2), type = "area", legend.pos = "bottomleft")
    eafplot(list(A1 = A1, A2 = A2), type = "point")
    eafplot(list(A1 = A1, A2 = A2), type = "point", pch = 20)
  } else {
    A1m <- A1; A1m[, which(maximise)] <- -A1m[, which(maximise)]
    A2m <- A2; A2m[, which(maximise)] <- -A2m[, which(maximise)]
    # FIXME: Colors are wrong
    eafplot(A1m, type = "area", maximise = maximise, legend.pos = "bottomleft")
    eafplot(A1m, type = "point", maximise = maximise)
    eafplot(A1m, type = "point", pch = 20, maximise = maximise)
    eafplot(list(A1m = A1m, A2m = A2m), type = "area", maximise = maximise, legend.pos = "bottomleft")
    eafplot(list(A1m = A1m, A2m = A2m), type = "point", maximise = maximise)
    eafplot(list(A1m = A1m, A2m = A2m), type = "point", pch = 20, maximise = maximise)
  }
  return(TRUE)
}
eaftest2 <- function()
{
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
 
  data(SPEA2minstoptimeRichmond)
  SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
  eafplot (SPEA2minstoptimeRichmond, xlab = expression(C[E]),
           ylab = "Minimum idle time (minutes)",
           las = 1, log = "y", maximise = c(FALSE, TRUE), main = "SPEA2 (Richmond)")
  return(TRUE)
}

expect_true(eaftest("wrots_l10w100_dat", "wrots_l100w10_dat"))
expect_true(eaftest("tpls", "rest"))
expect_true(eaftest("ALG_1_dat", "ALG_2_dat"))
expect_true(eaftest("ALG_1_dat", "ALG_2_dat", maximise = c(TRUE, FALSE)))
expect_true(eaftest("ALG_1_dat", "ALG_2_dat", maximise = c(FALSE, TRUE)))
expect_true(eaftest("ALG_1_dat", "ALG_2_dat", maximise = c(TRUE, TRUE)))
expect_true(eaftest2())
dev.off()
})
