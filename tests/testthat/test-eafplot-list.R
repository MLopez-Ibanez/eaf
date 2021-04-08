context("eafplot")
source("helper-common.R")

test_that("eafplot", {
skip_on_cran()
pdf(file = "eafplot.pdf", title = "eafplot.pdf", width = 9,  height = 6)
a <- "wrots_l10w100_dat"
b <- "wrots_l100w10_dat"
A1 <- read_datasets(file.path(system.file(package="eaf"), "extdata", a))
A2 <- read_datasets(file.path(system.file(package="eaf"), "extdata", b))
n <- min(nrow(A1), nrow(A2))
A1 <- A1[1:n, ]
A2 <- A2[1:n, ]
eafplot(list(A1 = A1, A2 = A2), type = "area", legend.pos = "bottomleft")
eafplot(list(A1 = A1, A2 = A2), type = "point")
expect_true(TRUE)
dev.off()
})
