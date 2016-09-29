# BSD_2_clause

#' A Search Engine for Endangered Species Act Documents
#'
#' @section About:
#'
#' Thousands of documents are created under the U.S. Endangered Species
#' Act each year, and thousands already exist. But too many are only available
#' as image-based PDFs that cannot be indexed and searched. This package
#' creates a Shiny app (and some helpers) for an Elastic-based search engine for
#' ESA documents.
#'
#' @section Document source:
#'
#' The documents in the Elastic database come first from
#' \href{http://ecos.fws.gov}{ECOS} (Fish and Wildlife Service; FWS) and from
#' National Marine Fisheries Service (NMFS) Office of Protected Resources (OPR)
#' websites. Most of these documents were acquired using
#' \link[ecosscraper]{ecosscraper}. In addition, we actively browse the internet
#' looking for other sites or pages with ESA-related documents.
#'
#' All of the documents we acquire are analyzed using \link[pdftext]{pdftext},
#' which either extracts text from the text layer (if available) or performs
#' optical character recognition (OCR) using \href{https://github.com/tesseract-ocr/tesseract}{Tesseract}.
#' The extracted text is loaded into Elastic using this package, and links to
#' the PDFs (which contain tables and figures not in the plain text extraction)
#' are provided in the app.
#'
#' @importFrom dplyr left_join bind_rows
#' @import elastic
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_replace_all
#'
#' @docType package
#' @aliases esadocs-package
#' @author Jacob Malcom \email{jmalcom@defenders.org}
#' @name esadocs
NULL
