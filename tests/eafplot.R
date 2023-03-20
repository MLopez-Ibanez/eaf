source("testthat/helper-common.R")
pdf(file = "eafplot.pdf", title = "eafplot.pdf", width = 6,  height = 6)
data(gcp2x2)
tabucol <- subset(gcp2x2, alg != "TSinN1")
tabucol$alg <- tabucol$alg[drop = TRUE]
eafplot(time + best ~ run, data = tabucol, subset = tabucol$inst == "DSJC500.5")

eafplot(time + best ~ run | inst, groups = alg, data = gcp2x2)

eafplot(time + best ~ run | inst, groups = alg, data = gcp2x2, percentiles = c(0,
    50, 100), include.extremes = TRUE, cex = 1.4, lty = c(2, 1, 2), lwd = c(2, 2,
    2), col = c("black", "blue", "grey50"))

dev.off()
