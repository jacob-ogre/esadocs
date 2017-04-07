# BSD_2_clause

library(dplyr)
library(elastic)
library(jsonlite)

# body <- fromJSON(paste(readLines("~/Data/ESAdocs_searches/searches_mapping.json"),
#                        collapse = "\n"))
body <- fromJSON(paste(readLines("inst/extdata/searches_mapping.json"),
                       collapse = "\n"))

connect()

if(index_exists("searches")) {
  index_delete("searches")
}
index_create("searches", body = body)

presearch <- readRDS("inst/extdata/presearch_2017-04-05.rds")
pre <- select(presearch, search_term, date)

load_res <- docs_bulk(
  pre,
  index = "searches",
  type = "basic"
)

res <- Search("searches", type = "basic", size = 500, asdf = TRUE)
r2 <- res$hits$hits$`_source`

fuz <- list(
  query = list(
    fuzzy = list(
      searches = "wolf"
    )
  )
)

ftst <- Search("searches", type = "basic", body = fuz, asdf = TRUE)
