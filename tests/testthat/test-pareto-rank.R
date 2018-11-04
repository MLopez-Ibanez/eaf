library(eaf)
context("pareto")

test_that("pareto", {
  test_pareto_rank <- function(extdatafile, maximise = FALSE) {
    path <- file.path(system.file(package="eaf"),"extdata", extdatafile)
    set <- read_datasets(path)
    # Drop set column
    set <- set[,-ncol(set)]
    ranks <- pareto_rank(set, maximise = maximise)
    set2 <- set
    for (r in min(ranks):max(ranks)) {
      # We have to keep_weakly because pareto_rank does the same.
      nondom <- is_nondominated(set2, maximise = maximise, keep_weakly = TRUE) 
      expect_equal(set[ranks == r, , drop = FALSE], set2[nondom, , drop = FALSE])
      set2 <- set2[!nondom, , drop = FALSE]
    }
  }
  test_pareto_rank("ALG_2_dat")
  test_pareto_rank("spherical-250-10-3d.txt")
})
