# BSD_2_clause

###############################################################################
# While basic searches appear to be working, it appears that short searches
# in particular are prone to extreme slowness and probably huge memory loads.
# This script works through searches to try to find better solutions.

library(dplyr)
library(elastic)
library(esadocs)
library(microbenchmark)

connect()

# using query bools
q1 <- '{
  "query": {
    "bool": {
      "must":     { "match": { "raw_txt": "turtle" }},
      "must_not": { "match": { "raw_txt": "tortoise"  }},
      "should": [
        { "match": { "raw_txt": "green" }},
        { "match": { "raw_txt": "sea"   }}
        ]
    }
  }
}'

index_clear_cache("esadocs")
system.time({
  r1 <- Search("esadocs", body = q1)$hits$hits
  r1df <- result_asdf(r1)
})
dim(r1df)
r1df$type
r1df$title

# compare to bool...looks like the timing is a bit faster
q2 <- '{
  "query": {
    "match": {
      "raw_txt": "green sea turtle NOT tortoise"
    }
  }
}'

index_clear_cache("esadocs")
system.time({
  r2 <- Search("esadocs", body = q2)$hits$hits
  r2df <- result_asdf(r2)
})
dim(r2df)

# A slow case...see q4, because it's highlighting that's slowing the query
q3 <- '{
  "min_score": 0.05,
  "query": {
    "match": {
      "raw_txt": "\\\"recovery unit\\\""
    }
  },
  "size": "500",
  "highlight": {
    "fields": {
      "raw_txt": {
        "type": "fvh",
        "fragment_size": "150",
        "pre_tags": ["<b>"],
        "post_tags": ["</b>"]
      }
    }
  }
}'

fgh <- index_clear_cache("esadocs")
system.time({
  r3 <- Search("esadocs",
               type = "recovery_plan",
               analyzer = "esadocs_analyzer",
               body = q3)$hits$hits
  r3df <- result_asdf(r3)
})
dim(r3df)
r3df$highlight <- get_highlight(r3)

# This shows that it's the normal highlighter that is slowing it down
q4 <- '{
  "min_score": 0.1,
  "query": {
    "match": {
      "raw_txt": "\\"recovery unit\\""
    }
  },
  "size": "500"
}'

index_clear_cache("esadocs")
system.time({
  r4 <- Search("esadocs", type = "recovery_plan", body = q4)$hits$hits
  r4df <- result_asdf(r4)
})
dim(r4df)
r4df$highlight <- get_highlight(r4)


body <- list(
      inline = list(
                 min_score = 0.1,
                 query = list(
                   match = list(
                     `{{my_field}}` = "{{my_value}}"
                    )
                 ),
                 size = "{{my_size}}",
                 highlight = list(
                   fields = list(
                     `{{my_field}}` = list(
                       `type` = "fvh",
                       `fragment_size` = 150,
                       `pre_tags` = list("<b>"),
                       `post_tags` = list("</b>")
                     )
                   )
                 )
               ),
      params = list(my_field = "raw_txt",
                    my_value = "\\\"recovery unit\\\"",
                    my_size = 500)
    )
Search_template_render(body)
cur_mats <- Search_template(body = body)$hits$hits
res <- result_asdf(cur_mats)
dim(res)


body <- list(
          min_score = 0.1,
          query = list(
            match = list(
              raw_txt = "\\\"recovery unit\\\""
             )
          ),
          size = 500,
          highlight = list(
            fields = list(
              raw_txt = list(
                `type` = "fvh",
                `fragment_size` = 150,
                `pre_tags` = list("<b>"),
                `post_tags` = list("</b>")
              )
            )
          )
)
cur_mats <- Search("esadocs", type = "recovery_plan", body = body)$hits$hits
res <- result_asdf(cur_mats)
dim(res)

body <- list(
          min_score = 0.1,
          query = list(
            match = list(
              raw_txt = "recovery unit"
             )
          ),
          size = 500,
          highlight = list(
            fields = list(
              raw_txt = list(
                `type` = "fvh",
                `fragment_size` = 150,
                `pre_tags` = list("<b>"),
                `post_tags` = list("</b>")
              )
            )
          )
)

system.time({
cur_mats <- Search("esadocs",
                   type = "",
                   analyzer = "esadocs analyzer",
                   body = body)$hits$hits
res <- result_asdf(cur_mats)
})
dim(res)

body <- list(
          min_score = 0.1,
          query = list(
            match = list(
              raw_txt = "\\\"recovery unit\\\""
             )
          ),
          size = 500
)

system.time({
cur_mats <- Search("esadocs",
                   type = "",
                   analyzer = "esadocs analyzer",
                   body = body)$hits$hits
res <- result_asdf(cur_mats)
})
dim(res)
