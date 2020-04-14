context("eafdiffplot")
source("helper-common.R")

test_that("eafdiffplot", {
skip_on_cran()
pdf(file = "eafdiffplot.pdf", title = "eafdiffplot.pdf", width = 9,  height = 6)
## FIXME: Add main=invokation
## FIXME: We need smaller data!
eaftest <- function(a, b, maximise = c(FALSE, FALSE)) {
  A1 <- read_datasets(file.path(system.file(package="eaf"), "extdata", a))
  A2 <- read_datasets(file.path(system.file(package="eaf"), "extdata", b))
  A1m <- A1; A1m[, which(maximise)] <- -A1m[, which(maximise)]
  A2m <- A2; A2m[, which(maximise)] <- -A2m[, which(maximise)]
  eafdiffplot(A1m, A2m, type = "area", maximise = maximise)
  eafdiffplot(A1m, A2m, type = "point", maximise = maximise)
  eafdiffplot(A1m, A2m, full.eaf = TRUE, maximise = maximise)
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
