# BSD_2_clause

#' Prepare Federal Register data for loading to Elastic
#'
#' @param docs A data.frame of basic doc title, link, species information
#' @param dates A data.frame similar to \code{docs}, with doc dates
#' @return what is returned
#' @seealso \link{bulk_recplan_prep} \link{bulk_fiveyr_prep}
#'  \link{bulk_consultation_prep}
#' @export
#' @examples
#' #one or more lines to demo the function
bulk_fedreg_prep <- function(docs, dates) {
  spp_links <- aggregate(species ~ Doc_Link, data = docs, FUN = unique)
  df <- left_join(spp_links, dates, by = c("Doc_Link")) %>%
          distinct(species, .keep_all = TRUE) %>%
          select(-Species)
  df$type <- rep(docs$type[1], length(df[,1]))
  df$pdf <- unlist(lapply(lapply(df$Doc_Link, make_file_paths), `[[`, 1))
  df$txt <- unlist(lapply(lapply(df$Doc_Link, make_file_paths), `[[`, 2))
  df$pdf_path <- paste0("~/esadocs/", df$type, "/PDFs/", df$pdf)
  df$txt_path <- paste0("~/esadocs/", df$type, "/TXTs/", df$txt)
  df$Date <- as.Date(df$Date)
  names(df) <- c("href", "species", "date", "fr_citation_page", "title",
                 "type", "pdf", "txt", "pdf_path", "txt_path")
  message("Starting MD5 calculations...")
  df$pdf_md5 <- md5sum(normalizePath(df$pdf_path))
  df$pdf_size <- file.size(normalizePath(df$pdf_path))
  return(df)
}

#' Prepare recovery plan data for loading to Elastic
#'
#' @param docs A data.frame of basic doc title, link, species information
#' @param dates A data.frame similar to \code{docs}, with doc dates
#' @return what is returned
#' @seealso \link{bulk_fedreg_prep} \link{bulk_fiveyr_prep}
#'  \link{bulk_consultation_prep}
#' @export
#' @examples
#' #one or more lines to demo the function
bulk_recplan_prep <- function(docs, dates) {
  spp_links <- aggregate(species ~ Doc_Link, data = docs, FUN = unique)
  df <- left_join(spp_links, dates, by = c("Doc_Link")) %>%
          distinct(species, .keep_all = TRUE) %>%
          select(-Species, -`Plan Action Status`)
  names(df) <- c("href", "species", "date", "title", "plan_status")
  df$type <- rep(docs$type[1], length(df[,1]))
  df$pdf <- unlist(lapply(lapply(df$href, make_file_paths), `[[`, 1))
  df$txt <- unlist(lapply(lapply(df$href, make_file_paths), `[[`, 2))
  df$pdf_path <- paste0("~/esadocs/", df$type, "/PDFs/", df$pdf)
  df$txt_path <- paste0("~/esadocs/", df$type, "/TXTs/", df$txt)
  df$date <- as.Date(df$date)
  message("Starting MD5 calculations...")
  df$pdf_md5 <- md5sum(normalizePath(df$pdf_path))
  df$pdf_size <- file.size(normalizePath(df$pdf_path))
  return(df)
}

#' Prepare five-year review data for loading to Elastic
#'
#' @param docs A data.frame of basic doc title, link, species information
#' @param dates A data.frame similar to \code{docs}, with doc dates included
#' @return what is returned
#' @seealso \link{bulk_fedreg_prep} \link{bulk_recplan_prep}
#'  \link{bulk_consultation_prep}
#' @export
#' @examples
#' #one or more lines to demo the function
bulk_fiveyr_prep <- function(docs, dates) {
  spp_links <- aggregate(species ~ Doc_Link, data = docs, FUN = unique)
  df <- left_join(spp_links, dates, by = c("Doc_Link")) %>%
          distinct(species, .keep_all = TRUE) %>%
          select(-Species)
  names(df) <- c("href", "species", "date", "title")
  df$type <- rep(docs$type[1], length(df[,1]))
  df$pdf <- unlist(lapply(lapply(df$href, make_file_paths), `[[`, 1))
  df$txt <- unlist(lapply(lapply(df$href, make_file_paths), `[[`, 2))
  df$pdf_path <- paste0("~/esadocs/", df$type, "/PDFs/", df$pdf)
  df$txt_path <- paste0("~/esadocs/", df$type, "/TXTs/", df$txt)
  df$date <- as.Date(df$date)
  message("Starting MD5 calculations...")
  df$pdf_md5 <- md5sum(normalizePath(df$pdf_path))
  df$pdf_size <- file.size(normalizePath(df$pdf_path))
  return(df)
}

#' Add raw text to a data.frame for loading to Elastic
#'
#' We use text extracted from PDFs with \link[pdftext]{pdftext} for full-text
#' search with Elastic, added in a single field, \code{raw_txt}. The volume of
#' data in \code{raw_txt} can be rather large; we recommend that it is added
#'
#' @param df A data.frame from a \code{bulk_*_prep} function, with txt_path var
#' @return df, with a raw_txt variable filled using readLines
#' @export
#' @examples
#' one or more lines to demo the function
add_raw_txt <- function(df) {
  df$raw_txt <- unlist(lapply(df$txt_path, load_doc_text))
  return(df)
}

chunked_es_loading <- function(df, index = "esadocs", type) {
  connect()
  brks <- seq(1, length(df[, 1]), 100)
  for(i in 1:length(brks)) {
    st <- brks[i]
    en <- ifelse(brks[i] + 99 < length(df[, 1]),
                 brks[i] + 99,
                 length(df[, 1]))
    cur_tst <- add_raw_txt(df[st:en, ])
    bulk <- docs_bulk(cur_tst, index = index, type = type)
    message(sprintf("Added records %s to %s\n", st, en))
  }
}