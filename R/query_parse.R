# BSD_2_clause

#' Prepare a user's query for elasticsearch
#'
#' @param q The input query
#' @return Query formatted for query_string or simple_query_string
#' @export
build_query <- function(q) {
  if(stringr::str_match_all(q, "\"")) {
    # parse quoted string
  }
}
