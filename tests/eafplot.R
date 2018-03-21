library(eaf)
pdf(file = "eafplot.pdf", title = "eafplot.pdf", width = 6,  height = 6)
data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop = TRUE]
eafplot(time + best ~ run, data = tabucol, subset = tabucol$inst == "DSJC500.5")

eafplot(time + best ~ run | inst, groups = alg, data = gcp2x2)

eafplot(time + best ~ run | inst, groups = alg, data = gcp2x2, percentiles = c(0,
    50, 100), include.extremes = TRUE, cex = 1.4, lty = c(2, 1, 2), lwd = c(2, 2,
    2), col = c("black", "blue", "grey50"))

A1 <- read.data.sets(file.path(system.file(package = "eaf"), "extdata", "ALG_1_dat"))
A2 <- read.data.sets(file.path(system.file(package = "eaf"), "extdata", "ALG_2_dat"))
eafplot(A1, A2, percentiles = c(50))

eafplot(list(A1 = A1, A2 = A2), percentiles = c(50))

eafplot(A1, type="area", legend.pos="bottomleft")

## Using extra.points
data(HybridGA)
data(SPEA2relativeVanzyl)
eafplot(SPEA2relativeVanzyl, percentiles = c(25, 50, 75), xlab = expression(C[E]),
    ylab = "Total switches", xlim = c(320, 400), extra.points = HybridGA$vanzyl,
    extra.legend = "Hybrid GA")

data(SPEA2relativeRichmond)
eafplot(SPEA2relativeRichmond, percentiles = c(25, 50, 75), xlab = expression(C[E]),
    ylab = "Total switches", xlim = c(90, 140), ylim = c(0, 25), extra.points = HybridGA$richmond,
    extra.lty = "dashed", extra.legend = "Hybrid GA")

data(SPEA2minstoptimeRichmond)
SPEA2minstoptimeRichmond[, 2] <- SPEA2minstoptimeRichmond[, 2] / 60
eafplot(SPEA2minstoptimeRichmond, xlab = expression(C[E]), ylab = "Minimum idle time (minutes)",
    las = 1, log = "y", maximise = c(FALSE, TRUE), main = "SPEA2 (Richmond)")

eafplot(A1, A2, percentiles = c(50))

dev.off()
