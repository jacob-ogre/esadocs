# BSD_2_clause

#' Extract a data.frame from Elastic query results
#'
#' Elastic query results are returned as a complex nested list that is
#' reasonably well-structured but a pain to parse. The basic \code{Search}
#' function of \link[elastic]{elastic} has a flag, \code{asdf}, to return the
#' results as a data.frame, but there is no generic function to extract the
#' data.frame. This function is tailored to the esadocs index, used in testing,
#' which includes a field (Scientific_Name) that is an array in elasticsearch
#' (list in R) and doesn't coerce to a single variable just using
#' \code{as.data.frame(res)}.
#'
#' @param res An elasticsearch query result
#' @return A data.frame of document results
#' @importFrom dplyr bind_rows
#' @export
result_asdf <- function(res) {
  score_ls <- vector("list", length(res))
  res_ls <- vector("list", length(res))
  for(i in 1:length(res)) {     # NOTE: lapply doesn't work in this case
    score_ls[[i]] <- res[[i]]$`_score`
    spp_tmp <- paste(res[[i]]$`_source`$species, collapse = "<br>")
    n_vars <- length(res[[i]]$`_source`)
    href <- res[[i]]$`_source`$href
    rest <- res[[i]]$`_source`[3:n_vars]
    cur_dat <- data.frame(href = href,
                          rest,
                          species = spp_tmp,
                          stringsAsFactors = FALSE)
    res_ls[[i]] <- cur_dat
  }
  res_df <- suppressWarnings(dplyr::bind_rows(res_ls))
  res_df$score <- unlist(score_ls)
  return(res_df)
}

#' Return highlighted text from elasticsearch results
#'
#' Highlighting the matched text can be used in displaying the search
#' results, much as The Google does.
#'
#' @param res An Elastic result object
#' @return A vector of highlighted matches
#' @export
get_highlight <- function(res) {
  res_ls = vector("list", length(res))

  # unfortunately, lapply doesn't seem to work well over an ES result object,
  # at least not without getting way more complicated than using a for loop
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

abbrev <- function(x) {
  if(length(x) > 3) {
    paste(x[1:3], collapse = " ... ")
  } else {
    paste(x, collapse = " ... ")
  }
}

