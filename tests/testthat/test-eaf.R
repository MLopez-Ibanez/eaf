library(eaf)
context("eaf")

extdata.path <- function(file)
  return(file.path(system.file(package = "eaf"), "extdata", file))

test_that("eaf", {
  #skip_on_cran()
  test.eaf.dataset <- function(name, percentiles = NULL) {
    dataset <- get(name)
    x <- eaf:::compute.eaf(dataset, percentiles)
    x[,3] <- floor(x[,3])
    #saveRDS(x, paste0(name, "-eaf.rds"))
    return(x)
  }
  test.eaf.file <- function(file, percentiles = NULL) {
    dataset <- read.data.sets(file)
    x <- eaf:::compute.eaf(dataset, percentiles)
    # FIXME: work-around for change in the computation
    x[,3] <- floor(x[,3])
    #saveRDS(x, paste0(basename(file), "-eaf.rds"))
    return(x)
  }
  expect_equal(test.eaf.file(extdata.path("ALG_1_dat")),
               readRDS("ALG_1_dat-eaf.rds"))
  expect_equal(test.eaf.dataset("SPEA2relativeRichmond"),
               readRDS("SPEA2relativeRichmond-eaf.rds"))
})
