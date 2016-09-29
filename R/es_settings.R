# BSD_2_clause

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
make_es_settings <- function(analyzers = list(), mappings = list()) {
  all_analyze <- paste(analyzers, collapse = ",")
  all_mapping <- paste(mappings, collapse = ",")
  # print(length(all_analyze))
  # return(all_mapping)
  if(length(analyzers) > 0 & length(mappings) > 0) {
    sets <- paste0('{\n"settings": ',
                   all_analyze,
                   ', "mappings": {',
                   all_mapping, "}\n}")
  # } else if(length(analyzers) > 0 & length(mappings) == 0) {
  #   sets <- paste('{"settings": ', all_analyze, "}")
  # } else if(length(analyzers) == 0 & length(mappings) > 0) {
  #   sets <- paste('{"mappings": ', all_mapping, "}")
  } else {
    print("Huh?")
  }
  return(sets)
}
