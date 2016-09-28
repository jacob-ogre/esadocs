# BSD_2_clause

#' Make elasticsearch index and search settings object
#'
#' <Need to add more...>
#'
#' @param analyzers A list of zero or more analyzers to include in settings
#' @param mappings A list of one or more document mappings to include
#' @return  A list of elasticsearch settings
#' @seealso \link{esadocs_analyzer} \link{fed_reg_mapping}
#' @export
#' @examples
#' make_es_settings(analyzers = list(esadocs_analyzer()),
#'                  mappings = list(fed_ref_mapping(), rec_plan_mapping()))
make_es_settings <- function(analyzers = list(),
                             mappings = list()) {
  sets <- list(
    settings = list(
      analyzers
    ),
    mappings = list(
      mappings
    )
  )
  return(sets)
}

#' Make elasticsearch index and search settings object from JSONs
#'
#' <Need to add more...>
#'
#' @param analyzers A list of zero or more analyzers to include in settings
#' @param mappings A list of one or more document mappings to include
#' @return  A list of elasticsearch settings
#' @seealso \link{esadocs_analyzer} \link{fed_reg_mapping}
#' @export
#' @examples
#' make_es_settings(analyzers = list(esadocs_analyzer()),
#'                  mappings = list(fed_ref_mapping(), rec_plan_mapping()))
alt_make_es_settings <- function(analyzers = list(),
                                 mappings = list()) {
  prefix <- '{"settings": '
  all_analyze <- ""
  all_mapping <- ""
  for(i in analyzers) {
    all_analyze <- paste(all_analyze, i)
  }
  for(i in mappings) {
    all_mapping <- paste(all_mapping, i)
  }
  if(length(analyzers) > 0 & length(mappings) > 0) {
    sets <- paste('{"settings": ',
                  all_analyze,
                  ', "mappings": ',
                  all_mapping, "}")
  } else if(length(analyzers) > 0 & length(mappings) == 0) {
    sets <- paste('{"settings": ', all_analyze, "}")
  } else if(length(analyzers) == 0 & length(mappings) > 0) {
    sets <- paste('{"mappings": ', all_mapping, "}")
  }
  return(sets)
}
