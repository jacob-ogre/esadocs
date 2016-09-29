# BSD_2_clause

#' Make elastic settings object from JSONs
#'
#' Better indexing and search with elastic requires customizing the settings
#' (e.g., analyzers) and document mappings. Both components are sent to elastic
#' in JSON, and we store the individual JSON entries in separate files in
#' inst/extdata. For example, \code{esadocs_analyzer.json} is a single object
#' for the app's elastic analyzer, and each document type (Fed. Reg. docs,
#' recovery plans, etc.) has its own JSON file.
#'
#' While I would like to just use one JSON parser across the entire esadocs
#' package, there's a hiccup. \code{jsonlite} is actively maintained and has a
#' public GitHub repo, but it handles terminal JSON values in a way that elastic
#' doesn't like (converts all values to an array). In contrast, \code{RJSONIO},
#' which doesn't appear to be actively maintained, handles terminal values
#' 'correctly' for elastic. So we have to use it here.
#'
#' @param analyzers A vector of zero or more analyzers to include in settings
#' @param mappings A vector of one or more document mappings to include
#' @return  A JSON of elasticsearch settings
#' @seealso \link{load_es_json}
#' @examples
#' analyzer <- load_es_json("data-raw/esadocs_analyzer.json")
#' fedreg <- load_es_json("data-raw/federal_register_mapping.json")
#' recplan <- load_es_json("data-raw/recovery_plan_mapping")
#' make_es_settings(analyzers = c(analyzer),
#'                  mappings = c(fedreg, recplan)
make_es_settings <- function(analyzers = c(), mappings = c()) {
  if(length(analyzers) > 0 & length(mappings) > 0) {
    sets <- list(settings = analyzers, mappings = mappings)
    sets <- RJSONIO::toJSON(sets, pretty = TRUE)
  } else if(length(analyzers) > 0 & length(mappings) == 0) {
    sets <- list(settings = analyzers)
    sets <- RJSONIO::toJSON(sets, pretty = TRUE)
  } else if(length(analyzers) == 0 & length(mappings) > 0) {
    sets <- list(mappings = mappings)
    sets <- RJSONIO::toJSON(sets, pretty = TRUE)
  } else {
    stop("One or more analyzers or mappings needs to be specified.")
  }
  return(sets)
}

#' Load a JSON for elastic settings
#'
#' Creating custom settings, e.g., analyzers and mappings, for elastic is a
#' bit tedious. The \code{body} variable in many \code{elastic} package functions
#' can be either a nested list or a JSON. Getting the formatting correct using
#' lists has been challenging for all but the simplest scenarios, and we decided
#' to use JSON for storing settings. But that has its own problems, e.g.,
#' combining different mappings in a single settings call requires a round-about
#' treatment of the JSON files. This function is the first step to loading
#' settings JSONs, but can't be further wrapped because of the need to combine
#' multiple JSONs for a single settings call.
#'
#' @param file Path to the JSON file to be loaded
#' @return The contents of \code{file} as a list
#' @seealso \link{make_es_settings}
#' @examples
#' load_es_json("data-raw/recovery_plan_mapping")
load_es_json <- function(file) {
  res <-  jsonlite::fromJSON(paste(readLines(file), collapse = "\n"))
  return(res)
}
