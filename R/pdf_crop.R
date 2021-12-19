#' Remove whitespace margins from a PDF file
#'
#' Remove whitespace margins using <https://ctan.org/pkg/pdfcrop>
#' 
#' @param filename Filename of a PDF file to crop. The file will be overwritten.
#' @param mustWork If `TRUE`, then give an error if the file cannot be cropped.
#' @param pdfcrop Path to the `pdfcrop` utility.
#' @return Nothing
#' 
#' @examples
#' \dontrun{
#' extdata_path <- system.file(package = "eaf", "extdata")
#' A1 <- read_datasets(file.path(extdata_path, "wrots_l100w10_dat"))
#' A2 <- read_datasets(file.path(extdata_path, "wrots_l10w100_dat"))
#' pdf(file = "eaf.pdf", onefile = TRUE, width = 5, height = 4)
#' eafplot(list(A1 = A1, A2 = A2), percentiles = 50, sci.notation=TRUE)
#' dev.off()
#' pdf_crop("eaf.pdf")
#' }
#' @export
#' @md
pdf_crop <- function(filename, mustWork = FALSE, pdfcrop = Sys.which("pdfcrop"))
{
  if (!file.exists(filename)) {
      stop("PDF file", shQuote(filename), "not found")
  } else if (is.null(pdfcrop) || pdfcrop == "") {
      if (mustWork) {
        stop("pdfcrop not found!")
      } else {
        warning("pdfcrop not found, not cropping")
      }
  } else {
    system2(pdfcrop, c("--pdfversion 1.5", filename, filename),
            timeout = 60, stdout = FALSE, stderr = FALSE)
  }
}

