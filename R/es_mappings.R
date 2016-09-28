# BSD_2_clause

#' Elasticsearch document mapping for Federal Register documents
#'
#' Custom mapping of Federal Register variables in ESAdocs search. Most
#' variables remain \code{not_analyzed} to facilitate filtering.
#'
#' @param ... Additional properties to add to the mapping (not enabled yet)
#' @return A document mapping as a nested list.
#' @export
#' @examples
#' fed_reg_mapping()
fed_reg_mapping <- function() {
  fed_reg_map <- list(
    federal_register = list(
      properties = list(
        link = list(
          type = "string",
          index = "not_analyzed"
        ),
        type = list(
          type = "string",
          index = "not_analyzed"
        ),
        Date = list(
          type = "date",
          index = "not_analyzed"
        ),
        `Citation Page` = list(
          type = "string",
          index = "not_analyzed"
        ),
        Title = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        raw_txt = list(
          type = "string",
          index = "analyzed",
          analyzer = "esadocs_analyzer"
        ),
        pdf_md5 = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_size = list(
          type = "long",
          index = "not_analyzed"
        ),
        Species = list(
          type = "string",
          index = "not_analyzed"
        )
      )
    )
  )
  return(fed_reg_map)
}

#' @export
alt_fed_reg_mapping <- function() {
  paste(readLines("data/federal_register_mapping.json"), collapse = "\n")
}

#' Elasticsearch document mapping for ESA recovery plans
#'
#' Custom mapping of recovery plan variables in ESAdocs search. Most
#' variables remain \code{not_analyzed} to facilitate filtering.
#'
#' @param ... Additional properties to add to the mapping (not enabled yet)
#' @return A document mapping as a nested list.
#' @export
#' @examples
#' rec_plan_mapping()
rec_plan_mapping <- function() {
  rec_plan_map <- list(
    recovery_plan = list(
      properties = list(
        link = list(
          type = "string",
          index = "not_analyzed"
        ),
        type = list(
          type = "string",
          index = "not_analyzed"
        ),
        Date = list(
          type = "date",
          index = "not_analyzed"
        ),
        Title = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        raw_txt = list(
          type = "string",
          index = "analyzed",
          analyzer = "esadocs_analyzer"
        ),
        pdf_md5 = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_size = list(
          type = "long",
          index = "not_analyzed"
        ),
        Species = list(
          type = "string",
          index = "not_analyzed"
        )
      )
    )
  )
  return(rec_plan_map)
}

#' Elasticsearch document mapping for ESA five-year reviews
#'
#' Custom mapping of five-year review variables in ESAdocs search. Most
#' variables remain \code{not_analyzed} to facilitate filtering.
#'
#' @param ... Additional properties to add to the mapping (not enabled yet)
#' @return A document mapping as a nested list.
#' @export
#' @examples
#' fiveyr_mapping()
fiveyr_mapping <- function() {
  fiveyr_map <- list(
    five_year_review = list(
      properties = list(
        link = list(
          type = "string",
          index = "not_analyzed"
        ),
        type = list(
          type = "string",
          index = "not_analyzed"
        ),
        Date = list(
          type = "date",
          index = "not_analyzed"
        ),
        Title = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        raw_txt = list(
          type = "string",
          index = "analyzed",
          analyzer = "esadocs_analyzer"
        ),
        pdf_md5 = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_size = list(
          type = "long",
          index = "not_analyzed"
        ),
        Species = list(
          type = "string",
          index = "not_analyzed"
        )
      )
    )
  )
  return(fiveyr_map)
}

#' Elasticsearch document mapping for ESA consultation docs
#'
#' Custom mapping of consultation document variables in ESAdocs search. Most
#' variables remain \code{not_analyzed} to facilitate filtering.
#'
#' @param ... Additional properties to add to the mapping (not enabled yet)
#' @return A document mapping as a nested list.
#' @export
#' @examples
#' consult_mapping()
consult_mapping <- function() {
  consult_map <- list(
    s7_consultation = list(
      properties = list(
        link = list(
          type = "string",
          index = "not_analyzed"
        ),
        type = list(
          type = "string",
          index = "not_analyzed"
        ),
        Date = list(
          type = "date",
          index = "not_analyzed"
        ),
        Title = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        txt_path = list(
          type = "string",
          index = "not_analyzed"
        ),
        raw_txt = list(
          type = "string",
          index = "analyzed",
          analyzer = "esadocs_analyzer"
        ),
        pdf_md5 = list(
          type = "string",
          index = "not_analyzed"
        ),
        pdf_size = list(
          type = "long",
          index = "not_analyzed"
        ),
        Species = list(
          type = "string",
          index = "not_analyzed"
        )
      )
    )
  )
  return(consult_map)
}


