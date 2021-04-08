# eaf 2.1

* Improve documentation of `igd()`.

* Fix errors with single-point attainment surfaces.

* Fix bug in `eafplot.list()`.


# eaf 2.0

* `read_datasets()` is able to read files compressed with `xz`.

* `eafs()` and `eafdiff()` and the plotting functions using them now consume
  slightly less memory.

* New function `whv_hype()` to estimate weighted hypervolume using Monte-Carlo
  sampling.

* New functions `total_whv_rect()` and `whv_rect()` to compute weighted
  hypervolume with rectangular weighted regions.

* New functions `largest_eafdiff()`, `choose_eafdiffplot()` and
  `choose_eafdiff()` for converting EAF differences into weighted regions for
  calculating the weighted hypervolume.
      
* New function `avg_hausdorff_dist()` for computing the averaged Hausdorff distance.
   

# eaf 1.9-1

* Fixes to Makefiles for non-GCC compilers and parallel build setups.


# eaf 1.9

* Compute Vorob'ev threshold, expectation and deviation. Plots of the symmetric
  deviation.                                             (Mickael Binois)

* Non-integer EAF percentiles are computed correctly.

* Various aesthetic improvements in `eafplot()` and `eafdiffplot()`.

* Functions for computing hypervolume, hypervolume contributions, epsilon
  metric, IGD+, filter dominated points, and fast normalisation of ranges.

* Command-line tools for computing the above are installed in `system.file(package="eaf", "bin/")`.

* Online documentation available at: http://lopez-ibanez.eu/eaftools

* New `read_datasets()` replaces deprecated `read.data.sets()`.
  New parameter `text` of `read_datasets()`.

* New `eafdiff()` function for computing EAF differences.

* The `data.frame` method for `eafplot` has been removed. It had unexpected
  behavior and the default method handles `data.frame` already.

* Python script to compute EAF differences installed at `system.file(package="eaf", "scripts/eafdiff.py")`.


# eaf 1.8

* Development version moved to GitHub: https://github.com/MLopez-Ibanez/eaf

* Remove leading zeros from version number.

* New parameters `left.panel.last` and `right.panel.last` of `eafdiffplot()`.

* Export and document function `eafs()` to compute EAFs.

* `eafdiff.pl`: Handle `--colors=`, `--intervals=`.

* Fix crash in `eafplot()` if `sets` is a vector of strings.

* Reset layout in `eafdiffplot()`.

* Compute eaf for 3D.

* Added testthat testing framework.

* The documentation is now generated with Roxygen2.

* Entry points to C code are now properly registered.


# eaf 1.07

* Silence CRAN warning for GNU extensions in Makefiles in `inst/scripts/eaf/Makefile`.


# eaf 1.06

* Fix bug when automatically generating a legend in eafplot with the
  formula interface (Thanks to Bernd Bischl for reporting this)

* Improve handling of various newline character formats.

* Reduce memory consumption (up to four times less memory).

* `eafdiff.pl`: Mention option --legendpos=none to hide the legend.

* eafplot.pl: Add options --maximise and --xmaximise and --colors=.
  Fix bug with --area.

* `eafplot.default()` now requires two colors when `type=="area"` and a
  palette is interpolated between these two colors for all the
  different levels plotted (Thanks to Alexandre Quemy for the suggestion).


# eaf 1.05

 *  Implement `type = "area")` for `eafdiffplot()`. This is now the default.

    The idea for the algorithm to compute the areas was provided by Carlos M. Fonseca. The implementation uses R polygons, which some PDF viewers may have trouble rendering correctly (See https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-are-there-unwanted-borders).

    Plots will look correct when printed. To get the previous behavior use `eafdiffplot(, type = "point")`.

* Attempt to deal with DOS/Unix newlines correctly.

* eafplot.pl: Lots of cleanup.
  (--single, --output, --legend): New options.
  Default to PDF.

* eafdiff.pl: Many cleanups.
  EAF diff with type="area" is the default now.
  Parameter --cex replaced by --scale.
  Do not require ps2eps.
  Handle --obj1= better. Crop pdf before converting to png.
  (--noattsurfs): New option.
  (--output-dir,--output): New options.
  (--eps): New option. PDF is the default now.

* Force eafdiff plots to use a square plotting region.

* `read.data.sets()` normalizes paths, thus it works with files such as ~/file.

* Function eafdiffplot() handles percentiles=NA and grand.lines
  parameter.

* Fix bug with ranges and maximise=TRUE in eafdiffplot().

* Fix points.steps to work correctly for all values of maximise.

* Fix bug in  eafdiffplot(..., maximise=c(TRUE,FALSE), full.eaf = TRUE)

* Fix "log" parameter in eafplot and eafdiffplot.

* Avoid that eafplot.formula modifies global options.

* Add 'axes' parameter to eafplot.

* To avoid confusion, the arguments 'xaxis.side' and 'yaxis.side' of
  eafplot can only take values c("below", "above") and c("left",
  "right"), respectively.

* Add missing Makefile in inst/scripts/eaf/.

# eaf 1.04

* Fix issues with only one point in the EAF.

# eaf 1.03

* Handle maximise argument in eafdiffplot and --maximise command-line
  option in eafdiff.pl

* R/calls.R (eafplot.data.frame): Fix problem with main parameter.

# eaf 1.00

* Initial release available in CRAN.
