# BSD_2_clause

#' The base file name and corresponding text file name
#'
#' @param x The (string) file path
#' @return A list with the pdf and txt file basenames
#' @export
#' @examples
#' make_file_paths("this/is/a/file.pdf")
make_file_paths <- function(x) {
  base <- basename(x)
  if(stringr::str_detect(base, "pdf$")) {
    text <- stringr::str_replace(base, "pdf$", "txt")
  } else {
    text <- paste0(base, ".txt")
    base <- paste0(base, ".pdf")
  }
  return(list(pdf = base, text = text))
}

#' Return the raw text of a file or NA if file missing
#'
#' @param path The system path to the text file of interest
#' @return A single string of the contents of the text file
#' @export
#' @examples
#' \dontrun{
#' load_doc_text("path/to/file.txt")
#' }
load_doc_text <- function(path) {
  if(!file.exists(path)) return(NA)
  return(paste(readLines(path), collapse = " "))
}
