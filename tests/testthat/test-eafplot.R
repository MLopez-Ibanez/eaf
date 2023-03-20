source("helper-common.R")

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

expect_true(eaftest("wrots_l10w100_dat", "wrots_l100w10_dat"))
expect_true(eaftest("tpls", "rest"))
expect_true(eaftest("ALG_1_dat.xz", "ALG_2_dat.xz"))
expect_true(eaftest("ALG_1_dat.xz", "ALG_2_dat.xz", maximise = c(TRUE, FALSE)))
expect_true(eaftest("ALG_1_dat.xz", "ALG_2_dat.xz", maximise = c(FALSE, TRUE)))
expect_true(eaftest("ALG_1_dat.xz", "ALG_2_dat.xz", maximise = c(TRUE, TRUE)))
dev.off()
})

data(HybridGA)
 
test_that("eafplot SPEA2relativeVanzyl", {
  skip_on_cran()
  data(SPEA2relativeVanzyl)
  expect_snapshot_plot("SPEA2relativeVanzyl", {
    eafplot(SPEA2relativeVanzyl, percentiles = c(25, 50, 75), 
            xlab = expression(C[E]), ylab = "Total switches", xlim = c(320, 400),
            extra.points = HybridGA$vanzyl, extra.legend = "Hybrid GA")
  })

  expect_snapshot_plot("SPEA2relativeVanzyl-extra_points", {
    eafplot(SPEA2relativeVanzyl, percentiles = c(25, 50, 75), xlab = expression(C[E]),
            ylab = "Total switches", xlim = c(320, 400), extra.points = HybridGA$vanzyl,
            extra.legend = "Hybrid GA")
  })
})

test_that("eafplot SPEA2relativeRichmond", {
  skip_on_cran()
  data(SPEA2relativeRichmond)
  expect_snapshot_plot("SPEA2relativeRichmond", {
    eafplot (SPEA2relativeRichmond, percentiles = c(25, 50, 75),
             xlab = expression(C[E]), ylab = "Total switches",
             xlim = c(90, 140), ylim = c(0, 25),
             extra.points = HybridGA$richmond, extra.lty = "dashed",
             extra.legend = "Hybrid GA")
  })
  expect_snapshot_plot("SPEA2relativeRichmond-extra_points", {
    eafplot(SPEA2relativeRichmond, percentiles = c(25, 50, 75), xlab = expression(C[E]),
            ylab = "Total switches", xlim = c(90, 140), ylim = c(0, 25), extra.points = HybridGA$richmond,
            extra.lty = "dashed", extra.legend = "Hybrid GA")
  })
})

test_that("eafplot SPEA2minstoptimeRichmond", {
  skip_on_cran()
  data(SPEA2minstoptimeRichmond)
  SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
  expect_snapshot_plot("SPEA2minstoptimeRichmond", {
    eafplot (SPEA2minstoptimeRichmond, xlab = expression(C[E]),
             ylab = "Minimum idle time (minutes)",
             las = 1, log = "y", maximise = c(FALSE, TRUE), main = "SPEA2 (Richmond)")
  })
  expect_snapshot_plot("SPEA2minstoptimeRichmond-extra_points", {
    eafplot(SPEA2minstoptimeRichmond, xlab = expression(C[E]), ylab = "Minimum idle time (minutes)",
            las = 1, log = "y", maximise = c(FALSE, TRUE), main = "SPEA2 (Richmond)")
  })
})
