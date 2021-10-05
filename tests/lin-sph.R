# FIXME: Move this to testthat
# Test compute eaf3D differences.
library(eaf)
lin <- read_datasets("lin.S.txt")
sph <- read_datasets("sph.S.txt")

nobjs <- ncol(lin) - 1
nruns.left <- max(lin[, nobjs + 1])
data.combined <- sph
data.combined[, nobjs + 1] <- data.combined[, nobjs + 1] + nruns.left
data.combined <- rbind(lin, data.combined)

# This may stop working once we filter uninteresting values in the C code directly.
DIFF <- eaf:::compute.eafdiff.helper(data.combined, intervals = nruns.left)
x <- as.matrix(read.table("lin.S-sph.S-diff.txt.xz", header = FALSE))
dimnames(x) <- NULL
x[, nobjs + 1] <- x[, nobjs + 1] - x[, nobjs + 2]
stopifnot(all.equal(DIFF[, 1 : (nobjs + 1)], x[, 1 : (nobjs + 1)]))
