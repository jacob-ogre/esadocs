# BSD_2_clause

#' A helper function to find relevant PDFs in one directory and copy to another
#'
#' @param basepath A character string of the root path to be searched
#' @param target A character string of the path to which PDFs are copied
#' @return The number of files found and copied
#' @export
#' @examples
#' \dontrun{
#' gather_pdfs("~/pdf_dir", "~/new_pdf_dir")
#' }
gather_pdfs <- function(basepath, target) {
  pdfs <- dir(basepath, pattern = "*.pdf", recursive = TRUE)
  copy_f <- function(f, basepath, target) {
    src <- paste(basepath, f, sep = "/")
    tar <- paste(target, basename(f), sep = "/")
    res <- try(file.copy(from = src, to = tar))
    if(class(res) != "try-error") return(TRUE)
    return(FALSE)
  }
  copy_res <- unlist(lapply(pdfs,
                            FUN = copy_f,
                            basepath = basepath,
                            target = target))
  sum(copy_res)
}

rand_files_copy <- function(src, dest, n = 10) {
  fils <- dir(src)
  selected <- sample(fils, n, replace = FALSE)
  copy_f <- function(f, src, dest) {
    s <- paste(src, f, sep = "/")
    d <- paste(dest, f, sep = "/")
    file.copy(s, d)
  }
  nil <- lapply(selected,
                FUN = copy_f,
                src = src,
                dest = dest)
}

copy_pdf_analogs <- function(txt_src, pdf_src, dest, n = 10) {
  fils <- dir(txt_src, pattern = "txt")
  pdfs <- gsub(fils, pattern = "txt$", replacement = "pdf", fixed = FALSE)
  fils <- paste(txt_src, fils, sep = "/")
  dests <- paste(dest, pdfs, sep = "/")
  pdfs <- paste(pdf_src, pdfs, sep = "/")
  for(i in 1:length(pdfs)) {
    if(file.exists(pdfs[i])) {
      file.copy(pdfs[i], dests[i])
    } else {
      warning(paste0(pdfs[i], " does not exist yet.\n\n"))
    }
  }
}
