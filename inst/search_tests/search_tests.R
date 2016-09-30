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
  "query": {
    "match": {
      "raw_txt": "\\"recovery unit\\""
    }
  },
  "size": "50",
  "highlight": {
    "fields": {
      "raw_txt": {
        "type": "fvh",
        "fragment_size": "50",
        "pre_tags": ["<b>"],
        "post_tags": ["</b>"]
      }
    }
  }
}'

index_clear_cache("esadocs")
system.time({
  r3 <- Search("esadocs", body = q3)$hits$hits
  r3df <- result_asdf(r3)
})
dim(r3df)
r3df$highlight <- get_highlight(r3)

# This shows that it's the normal highlighter that is slowing it down
q4 <- '{
  "query": {
    "match": {
      "raw_txt": "\\"recovery unit\\""
    }
  },
  "size": "50"
}'

index_clear_cache("esadocs")
system.time({
  r4 <- Search("esadocs", body = q4)$hits$hits
  r4df <- result_asdf(r4)
})
dim(r4df)
r4df$highlight <- get_highlight(r4)


