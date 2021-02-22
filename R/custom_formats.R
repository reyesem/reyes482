#' Custom Word template.
#'
#' Loads additional style file for Word for MA482.
#'
#' @param ... additional arguments provided to \@code{word_document}.
#'
#' @export
ma482_word_format <- function(...) {
  # locations of resource files in the package
  pkg_resource <- function(...) {
    system.file(..., package = "reyes482")
  }

  wordstyle <- pkg_resource("rmarkdown/resources/ReyesStyleDoc.docx")

  # call the base word_document function
  rmarkdown::word_document(
    reference_docx = wordstyle,
    ...
  )
}
