



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read one of the fonts included with this package
#'
#' @param font_name name of font
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_bdf_builtin <- function(font_name = c(
  "spleen-5x8.bdf",
  "spleen-6x12.bdf",
  "spleen-8x16.bdf",
  "spleen-12x24.bdf",
  "spleen-16x32.bdf",
  "spleen-32x64.bdf",
  "creep2-11.bdf",
  "cozette.bdf"
)) {

  if (is.numeric(font_name)) {
    font_name <- c(
      "spleen-5x8.bdf",
      "spleen-6x12.bdf",
      "spleen-8x16.bdf",
      "spleen-12x24.bdf",
      "spleen-16x32.bdf",
      "spleen-32x64.bdf",
      "creep2-11.bdf",
      "cozette.bdf"
    )[[font_name[1]]]
  } else {
    font_name <- match.arg(font_name)
  }

  filename  <- system.file(font_name, package = "bdftools", mustWork = TRUE)

  read_bdf(filename)
}
