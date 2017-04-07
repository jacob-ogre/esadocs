# BSD_2_clause

library(elastic)
library(jsonlite)

# body <- fromJSON(paste(readLines("inst/extdata/presearch_mapping.json"),
#                        collapse = "\n"))
body <- fromJSON(paste(readLines("~/Data/ESAdocs_presearch/presearch_mapping.json"),
                       collapse = "\n"))

connect()

if(index_exists("presearch")) {
  index_delete("presearch")
}
index_create("presearch", body = body)
res <- Search("presearch", type = "basic")
res$hits$hits

# res <- docs_create(
#   index = "presearch",
#   type = "basic",
#   id = 1234,
#   body = list(
#     search_term = "testing",
#     count = 1,
#     rds_path = "~/Downloads/test.txt",
#     date = as.Date("2017-03-30")
#   )
# )

res <- Search("presearch", type = "basic")

upd <- docs_update(
  index = "presearch",
  type = "basic",
  id = 1234,
  body = list(
    doc = list(count = res$hits$hits[[1]]$`_source`$count + 1)
  )
)

r2 <- Search("presearch", "basic", q = "search_term:'testing'")
r2$hits$hits
r3 <- Search("presearch", "basic", q = "search_term:'nothing'")
