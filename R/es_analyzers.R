# BSD_2_clause

#' Custom elasticsearch analyzer for ESAdocs 
#'
#' Building custom analyzers for elasticsearch is a bit of a pain: they either
#' need to be text JSON or nested lists. This will, hopefully, make it a little
#' easier to tweak a basic custom analyzer.
#'
#' @param tokenizer The type of tokenizer to use ['standard']
#' @param filter A list of \href{token filters}{https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-tokenfilters.html} for the analyzer ['standard',
#'   'lowercase', 'stop', 'cust_shingle']
#' @param min_shingle The minimum word n-gram length ['2']
#' @param max_shingle The maximum word n-gram length ['3']
#' @return A custom elasticsearch analyzer as a nested list
#' @export
#' @examples
#' esadocs_analyzer()
esadocs_analyzer <- function(tokenizer = "standard",
                             filter = list(
                               "standard",
                               "lowercase",
                               "stop",
                               "cust_shingle"),
                             min_shingle = "2",
                             max_shingle = "3") {
  if(!is.character(min_shingle)) min_shingle <- as.character(min_shingle)
  if(!is.character(max_shingle)) max_shingle <- as.character(max_shingle)
  new_analyzer <- list(
    analysis = list(
      analyzer = list(
        esadocs_analyzer = list(
          type = "custom",
          tokenizer = "standard",
          filter = filter
        )
      ),
      filter = list(
        cust_shingle = list(
          type = "shingle",
          min_shingle_size = min_shingle,
          max_shingle_size = max_shingle
        )
      )
    )
  )
  return(new_analyzer)
}
