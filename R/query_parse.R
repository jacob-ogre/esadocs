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

q33 <- '{
  "query": {
    "bool": {
      "should": {
        "match": {
          "raw_txt": "recovery unit"
        }
      },
      "must": {
        "match": {
          "raw_txt.shingles": "\\\"recovery unit\\\""
        }
      },
      "filter": {
        "range": {
          "date": {
            "gte": "2005-01-01",
            "lte": "2016-07-07"
          }
        }
      }
    }
  },
  "size": "500",
  "min_score": 0.1,
  "highlight": {
    "fields": {
      "raw_txt.shingles": {
        "type": "fvh",
        "fragment_size": "150",
        "pre_tags": ["<b>"],
        "post_tags": ["</b>"]
      }
    }
  }
}'
