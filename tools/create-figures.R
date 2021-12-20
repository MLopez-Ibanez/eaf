library(eaf)
extdata_dir <- system.file(package="eaf", "extdata")
A1 <- read_datasets(file.path(extdata_dir, "ALG_1_dat.xz"))
A2 <- read_datasets(file.path(extdata_dir, "ALG_2_dat.xz"))
# FIXME: How to crop PNG?
png(filename = "man/figures/ALG_1_dat-ALG_2_dat.png", width = 480, height = 240)
eafdiffplot(A1, A2, type = "point", title.left = "Algorithm 1", title.right = "Algorithm 2")
invisible(dev.off())

a1 <- read_datasets(file.path(extdata_dir, "wrots_l100w10_dat"))
a2 <- read_datasets(file.path(extdata_dir, "wrots_l10w100_dat"))
# FIXME: How to crop PNG?
png(filename = "man/figures/eafdiff-color.png", width = 600, height = 350)
eafdiffplot(a1, a2, col = colorRampPalette(c("blue", "red")), intervals = 10,
            title.left = expression("W-RoTS, " * lambda * "=" * 100 * ", " * omega * "=" * 10),
            title.right= expression("W-RoTS, " * lambda * "=" * 10 * ", " * omega * "=" * 100))
invisible(dev.off())
