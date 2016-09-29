# BSD_2_clause

#' description 1-liner
#'
#' More detailed description of the function
#' @param paramName explanation
#' @return what is returned
#' @seealso if any see alsos
#' @export
#' @examples
#' one or more lines to demo the function
bulk_ecosdocs_prep <- function(docs, dates, doctype) {
  spp_links <- aggregate(species ~ Doc_Link, data = docs, FUN = unique)
  df <- left_join(spp_links, dates, by = c("Doc_Link"))
  df <- distinct(df, species, .keep_all = TRUE)
  df <- select(df, -Species)
  df$doctype <- rep(doctype, length(df[,1]))
  df$pdf <- unlist(lapply(lapply(df$Doc_Link, make_file_paths), `[[`, 1))
  df$txt <- unlist(lapply(lapply(df$Doc_Link, make_file_paths), `[[`, 2))
  df$pdf_path <- paste0("~/esadocs/", df$doctype, "/PDFs/", df$pdf)
  df$txt_path <- paste0("~/esadocs/", df$doctype, "/TXTs/", df$txt)
  df$Date <- as.Date(df$Date)
  names(df) <- c("href", "species", "date", "fr_citation_page", "title",
                 "doctype", "pdf", "txt", "pdf_path", "txt_path")
  df$pdf_md5 <- md5sum(normalizePath(df$pdf_path))
  df$pdf_size <- file.size(normalizePath(df$pdf_path))
  df$raw_txt <- unlist(lapply(df$txt_path, load_doc_text))
  return(df)
}
