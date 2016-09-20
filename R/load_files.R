# BSD_2_clause

#' Make a JSON of ESA doc files for elasticsearch
#'
#' More detailed description of the function
#' @param txt_dir Path to the directory containing TXT files
#' @return what is returned
#' @seealso if any see alsos
#' @export
#' @examples
#' \dontrun{
#' make_json("~/txts", "~/pdfs")
#' }
make_json <- function(txt_dir, pdf_dir) {
  pdfs <- data.frame(pdf = dir(pdf_dir, pattern = "*.pdf", recursive = TRUE),
                     stringsAsFactors = FALSE)
  pdfs$basename <- stringr::str_replace(pdfs$pdf, "\\.pdf", "")
  pdfs$pdf <- paste0(pdf_dir, "/", pdfs$pdf)


  txts <- data.frame(txt = dir(txt_dir, pattern = "*.txt", recursive = TRUE),
                     stringsAsFactors = FALSE)
  txts$basename <- stringr::str_replace(txts$txt, "\\.txt", "")
  txts$txt <- paste0(txt_dir, "/", txts$txt)
  raw_txt <- unlist(lapply(txts$txt,
                           FUN = function(x) paste(readLines(x),
                                                   collapse = "\n")))

  dat <- dplyr::left_join(txts, pdfs, by = "basename")
  dat$raw_txt <- raw_txt
  return(dat)
}

