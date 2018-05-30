library(eaf)
context("hypervolume")

test_that("hypervolume", {
#skip_on_cran()
test.hv.rda <- function(dataset, reference, maximise = FALSE) {
  nobj <- length(reference)
  load(paste0(dataset, ".rda"))
  return(hypervolume(get(dataset)[,1:nobj], reference = reference, maximise))
}
test.hv.file <- function(file, reference, maximise = FALSE) {
  nobj <- length(reference)
  dataset <- read.data.sets(file)
  return(hypervolume(dataset[,1:nobj], reference = reference, maximise))
}

expect_equal(test.hv.rda("DTLZDiscontinuousShape.3d.front.1000pts.10",
                         reference = c(10,10,10)),
             719.223555475191)

expect_equal(test.hv.file("duplicated3.inp",
                          reference = c(-14324, -14906, -14500, -14654, -14232, -14093)),
             1.52890128312393e+20)
})
