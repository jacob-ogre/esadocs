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
  for(i in 1:length(res)) {     # NOTE: lapply doesn't work for some reason
    score_ls[[i]] <- res[[i]]$`_score`
    type <- get_var(res[[i]]$`_source`, "type")
    title <- get_var(res[[i]]$`_source`, "title")
    date <- get_var(res[[i]]$`_source`, "date")
    file_name <- get_var(res[[i]]$`_source`, "file_name")
    link <- get_var(res[[i]]$`_source`, "link")
    pdf_path <- get_var(res[[i]]$`_source`, "pdf_path")
    txt_path <- get_var(res[[i]]$`_source`, "txt_path")
    # raw_text <- get_var(res[[i]]$`_source`, "raw_txt")
    pdf_md5 <- get_var(res[[i]]$`_source`, "pdf_md5")
    n_pages <- get_var(res[[i]]$`_source`, "n_pages")
    geo <- get_var(res[[i]]$`_source`, "geo")
    tags <- get_var(res[[i]]$`_source`, "tags")
    fr_citation_page <-  get_var(res[[i]]$`_source`, "fr_citation_page")
    plan_act_status <-  get_var(res[[i]]$`_source`, "plan_act_status")
    plan_status <-  get_var(res[[i]]$`_source`, "plan_status")
    federal_agency <- get_var(res[[i]]$`_source`, "federal_agency")
    activity_code <-  get_var(res[[i]]$`_source`, "activity_code")
    ch_status <- get_var(res[[i]]$`_source`, "ch_status")
    doc_type <- get_var(res[[i]]$`_source`, "doc_type")
    services <- get_var(res[[i]]$`_source`, "services")
    spp_tmp <- paste(res[[i]]$`_source`$species, collapse = "<br>")
    cur_dat <- data.frame(type = type,
                          title = title,
                          date = date,
                          file_name = file_name,
                          link = link,
                          pdf_path = pdf_path,
                          txt_path = txt_path,
                          # raw_txt = raw_text,
                          pdf_md5 = pdf_md5,
                          n_pages = n_pages,
                          fr_citation_page = fr_citation_page,
                          federal_agency = federal_agency,
                          activity_code = activity_code,
                          ch_status = ch_status,
                          doc_type = doc_type,
                          species = spp_tmp,
                          geo = geo,
                          tags = tags,
                          stringsAsFactors = FALSE)
    # names(cur_dat)[7] <- "raw_txt"
    res_ls[[i]] <- cur_dat
  }
  res_df <- suppressWarnings(dplyr::bind_rows(res_ls))
  res_df$score <- unlist(score_ls)
  return(res_df)
}

get_var <- function(src, varname) {
  ifelse(is.null(src[[varname]]), NA, src[[varname]])
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

