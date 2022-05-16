#' Remove whitespace margins from a PDF file (and maybe embed fonts)
#'
#' Remove whitespace margins using <https://ctan.org/pkg/pdfcrop> and
#' optionally embed fonts using [grDevices::embedFonts()]. You may install
#' `pdfcrop` using TinyTeX (<https://cran.r-project.org/package=tinytex>) with
#' `tinytex::tlmgr_install('pdfcrop')`.
#'
#' You may also wish to consider [extrafont::embed_fonts()]
#' (<https://cran.r-project.org/package=extrafont>).
#'
#' ```
#' library(extrafont)
#' # If you need to specify the path to Ghostscript (probably not needed in Linux)
#' Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.56.1/bin/gswin64c.exe")
#' embed_fonts("original.pdf", outfile = "new.pdf")
#' ```
#'
#' As an alternative, saving the PDF with [grDevices::cairo_pdf()] should
#' already embed the fonts.
#' 
#' @param filename Filename of a PDF file to crop. The file will be overwritten.
#' @param mustWork If `TRUE`, then give an error if the file cannot be cropped.
#' @param pdfcrop Path to the `pdfcrop` utility.
#' @param embed_fonts (`logical(1)`) If `TRUE`, use [grDevices::embedFonts()] to embed fonts.
#' @return Nothing
#'
#' @seealso [grDevices::embedFonts()] [extrafont::embed_fonts()] [grDevices::cairo_pdf()]
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
pdf_crop <- function(filename, mustWork = FALSE, pdfcrop = Sys.which("pdfcrop"),
                     embed_fonts = FALSE)
{
  if (!file.exists(filename)) {
    stop("PDF file", shQuote(filename), "not found")
  } else if(!has_file_extension(filename, "pdf")){
    stop(shQuote(filename), "is not a PDF file")
  } else if (is.null(pdfcrop) || pdfcrop == "") {
      if (mustWork) {
        stop("pdfcrop not found!")
      } else {
        warning("pdfcrop not found, not cropping")
      }
  } else {
    system2(pdfcrop, c("--pdfversion 1.5", shQuote(filename), shQuote(filename)),
            timeout = 60, stdout = FALSE, stderr = FALSE)
    if (embed_fonts) embedFonts(filename)
  }
}
