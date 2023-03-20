source("helper-common.R")

test_that("eafplot list wrots", {
  skip_on_cran()
  a <- "wrots_l10w100_dat"
  b <- "wrots_l100w10_dat"
  A1 <- read_datasets(file.path(system.file(package="eaf"), "extdata", a))
  A2 <- read_datasets(file.path(system.file(package="eaf"), "extdata", b))
  n <- min(nrow(A1), nrow(A2))
  A1 <- A1[1:n, ]
  A2 <- A2[1:n, ]
  expect_snapshot_plot("wrots_area_bottomleft", {
    eafplot(list(A1 = A1, A2 = A2), type = "area", legend.pos = "bottomleft")
  })
  expect_snapshot_plot("wrots_point", {
    eafplot(list(A1 = A1, A2 = A2), type = "point")
  })
})

test_that("eafplot list ALG", {
  skip_on_cran()
  A1 <- read_extdata("ALG_1_dat.xz")
  A2 <- read_extdata("ALG_2_dat.xz")
  expect_snapshot_plot("ALG-50", {
    eafplot(list(A1 = A1, A2 = A2), percentiles = 50)
  })
  expect_snapshot_plot("ALG-area-bottomleft", {
    eafplot(A1, type="area", legend.pos="bottomleft")
  })
})
