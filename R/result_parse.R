# BSD_2_clause

#' Extract data.frame from elasticsearch query result
#'
#' Elasticsearch query results are returned as a complex nested list that is
#' reasonably well-structured but a pain to parse. The basic \code{Search}
#' function of \link[elastic]{elastic} has a flag, \code{asdf}, to return the
#' results as a data.frame, but there is no generic function to extract the
#' data.frame. This function is tailored to the esadocs2 index, used in testing,
#' which includes a field (Scientific_Name) that is an array in elasticsearch
#' (list in R) and doesn't coerce to a single variable just using
#' \code{as.data.frame}.
#'
#' @param res An elasticsearch query result
#' @return A data.frame of document results
#' @importFrom dplyr bind_rows
#' @export
result_asdf <- function(res) {
  score_ls <- list()
  res_ls <- list()
  for(i in 1:length(res)) {     # NOTE: lapply doesn't work in this case
    score_ls[[i]] <- res[[i]]$`_score`
    spp_tmp <- paste(res[[i]]$`_source`$Scientific_Name, collapse = "<br>")
    n_vars <- length(res[[i]]$`_source`)
    rest <- res[[i]]$`_source`[1:n_vars-1]
    cur_dat <- data.frame(rest, Species = spp_tmp, stringsAsFactors = FALSE)
    res_ls[[i]] <- cur_dat
  }
  res_df <- suppressWarnings(dplyr::bind_rows(res_ls))
  res_df$Score <- unlist(score_ls)
  return(res_df)
}

#' Return highlighted text from elasticsearch results
#'
#' Highlighting the matched text can be used in displaying the search
#' results, much as The Google does.
#'
#' @param res An elasticsearch result object
#' @return A vector of highlighted matches
#' @seealso \link{abbreviate}
#' @export
get_highlight <- function(res) {
  res_ls = list()
  for(i in 1:length(res)) {
    hi_tmp <- lapply(res[[i]]$highlight, FUN = abbrev)
    hi_tmp <- str_replace_all(hi_tmp,
                              "[ ]{2,}|\n",
                              " ")
    res_ls[[i]] <- hi_tmp
  }
  res_vec <- unlist(res_ls)
  return(res_vec)
}

#' Abbreviate multiple matches
#'
#' Concatenates with ' ... '
#'
#' @param x A vector of highlighted match strings
#' @return A concatenated version from <= 3 match elements
abbrev <- function(x) {
  if(length(x) > 3) {
    paste(x[1:3], collapse = "...")
  } else {
    paste(x, collapse = "...")
  }
}
