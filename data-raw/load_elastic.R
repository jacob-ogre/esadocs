# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(elastic)
library(tools)

data("ecos_doc_links")
data("TECP_domestic")
data("federal_register_table")

head(data.frame(ecos_doc_links))
ecos_doc_links <- filter(ecos_doc_links,
                         Scientific_Name %in% unique(TECP_domestic$Scientific_Name))
fedreg <- filter(ecos_doc_links, type == "federal_register")
recpln <- filter(ecos_doc_links, type == "recovery_plan")
fiveyr <- filter(ecos_doc_links, type == "five_year_review")
test <- rbind(head(fedreg), head(recpln), head(fiveyr))
names(test) <- c("Species", "Doc_Link", "link", "text", "type")
names(federal_register_table)

# First need to test that the loading will work as I think it will work
tmp <- head(fedreg, 50)
names(tmp) <- c("Species", "Doc_Link", "link", "text", "type")

##########
qq <- left_join(tmp, federal_register_table, by = c("Doc_Link", "Species"))
# Well crap...this test join highlights a problem from earlier (in ecosscraper)
# that I don't think I can get around. I haven't figured out a clean way to get
# all of the information in tables out without separate calls for the text (<a>)
# and for the links (href), then joining those tables on the text. When there
# are two different docs with the same Title then that doc/link gets double-
# counted. In this case, a proposed rule and a final rule have the same title,
# inflating the document count from 25 to 27. There is no way to pick the correct
# a:href pair to retain.
######

qq <- select(qq, -text)
qq$pdf <- unlist(lapply(lapply(qq$link, make_file_paths), `[[`, 1))
qq$txt <- unlist(lapply(lapply(qq$link, make_file_paths), `[[`, 2))
qq$pdf_path <- paste0("~/esadocs/", qq$type, "/PDFs/", qq$pdf)
qq$txt_path <- paste0("~/esadocs/", qq$type, "/TXTs/", qq$txt)
qq$raw_txt <- unlist(lapply(qq$txt_path, load_doc_text))
qq$pdf_md5 <- md5sum(normalizePath(qq$pdf_path))
qq$pdf_size <- file.size(normalizePath(qq$pdf_path))

# One missing field is the species that are covered by a given document
spp <- aggregate(Scientific_Name ~ href, data = fedreg, FUN = unique)
tt <- left_join(qq, spp, by = c("Doc_Link" = "href"))
rr <- select(tt, -Species)
ee <- distinct(rr, pdf, Title, .keep_all = TRUE)
ee <- select(ee, -Doc_Link)

# need to run this on next load so that we can filter by date
ee$Date <- as.Date(ee$Date)

## NGram tokenizer
body <- '{
  "settings" : {
    "analysis" : {
      "analyzer" : {
        "my_ngram_analyzer" : {
          "type": "custom",
          "tokenizer" : "standard",
          "filter": [
            "standard",
            "lowercase",
            "stop",
            "custom_shingle"
          ]
        }
      },
      "filter" : {
        "custom_shingle": {
          "type": "shingle",
          "min_shingle_size": "2",
          "max_shingle_size": "3"
        }
      }
    }
  },
  "mappings": {
    "federal_register": {
      "properties": {
        "link": {
          "type": "string",
          "index": "not_analyzed"
        },
        "type": {
          "type": "string",
          "index": "not_analyzed"
        },
        "Date": {
          "type": "date",
          "index": "not_analyzed"
        },
        "Date": {
          "type": "date",
          "index": "not_analyzed"
        },
        "Citation Page": {
          "type": "string",
          "index": "not_analyzed"
        },
        "Title": {
          "type": "string",
          "index": "not_analyzed"
        },
        "pdf": {
          "type": "string",
          "index": "not_analyzed"
        },
        "txt": {
          "type": "string",
          "index": "not_analyzed"
        },
        "pdf_path": {
          "type": "string",
          "index": "not_analyzed"
        },
        "txt_path": {
          "type": "string",
          "index": "not_analyzed"
        },
        "raw_txt": {
          "type": "string",
          "index": "analyzed",
          "analyzer": "my_ngram_analyzer"
        },
        "pdf_md5": {
          "type": "string",
          "index": "not_analyzed"
        },
        "pdf_size": {
          "type": "long",
          "index": "not_analyzed"
        },
        "Species": {
          "type": "string",
          "index": "not_analyzed"
        }
      }
    }
  }
}'

body1 <- make_es_settings(
  analyzers = list(esadocs_analyzer()),
  mappings = list(fed_reg_mapping())
)

hj <- alt_make_es_settings(analyzers = list(alt_esadocs_analyzer()),
                           mappings = list(alt_fed_reg_mapping()))
po <- jsonlite::fromJSON(hj)


# What the heck, why not try a test load to elasticsearch?
connect()
index_delete("esadocs2")
index_create("esadocs2", body = hj)
bulk <- docs_bulk(ee, index = "esadocs", type = "federal_register")

index_analyze(text = "Fish and Wildlife Service",
              index = "esadocs2")

ll <- Search(index = "esadocs",
             q = "Fish and Wildlife",
             analyzer = "esadocs_analyzer",
             size = 25)
ll <- Search(index = "esadocs2",
             q = "flower^5",
             analyzer = "my_ngram_analyzer",
             size = 25)
pl <- result_asdf(ll$hits$hits)

######################
# some search testing

body1 <- '{
  "inline" : {
    "query": {
      "simple_query_string": {
        "query": "{{my_value}}",
        "analyzer": "my_ngram_analyzer",
        "fields": [ "{{my_field}}" ],
        "default_operator": "and"
      }
    },
    "size" : "{{my_size}}",
    "highlight": {
      "fields": {
        "{{my_field}}" : {
          "fragment_size" : 150,
          "number_of_fragments" : 3
        }
      }
    }
  },
  "params" : {
    "my_field" : "raw_txt",
    "my_value" : "\\"fish and wildlife\\"",
    "my_size" : 20
  }
}'

body1 <- list(
  inline = list(
    query = list(
      simple_query_string = list(
        query = "{{my_value}}",
        analyzer = "my_ngram_analyzer",
        fields = list( "{{my_field}}" ),
        default_operator = "and"
      )
    ),
    size = "{{my_size}}",
    highlight = list(
      fields = list(
        `{{my_field}}` = list(
          fragment_size = 150,
          number_of_fragments = 3
        )
      )
    )
  ),
  params = list(
    my_field = "raw_txt",
    my_value = "fish and wildlife",
    my_size = 20
  )
)

Search_template_render(body = body1)
rr <- Search_template(body = body1)$hits$hits

length(rr)
dim(result_asdf(rr))
ghh <- result_asdf(rr)
ghh$highlight <- get_highlight(rr)

vv <- termvectors("esadocs2",
                  type = "federal_register",
                  id = "AVdr8968E9ijamFrGyQc",
                  fields = list("raw_txt"),
                  term_statistics = TRUE,
                  field_statistics = FALSE,
                  payloads = FALSE,
                  positions = FALSE)
terms <- names(vv$term_vectors$raw_txt$terms)
terms[1:1000]

############

body2 <- list(
  inline = list(query = list(match = list(`{{my_field}}` = "{{my_value}}")),
                size = "{{my_size}}",
                highlight = list(
                  fields = list(
                    `{{my_field}}` = list(
                      `fragment_size` = 150)
                    )
                  )
                ),
  params = list(my_field = "raw_txt",
                my_value = "flowers",
                my_size = 20L)
)
tt <- Search_template(body = body2)$hits$hits
t2 <- result_asdf(tt)

t2 <- Search(index = c("esadocs2", "esadocs"),
             q = "'default_field': 'raw_txt', 'query' = 'Viola'",
             size = 3,
             body = paste('"highlight" : { "fields" : { "{{my_field}}" :',
                    '{"fragment_size" : 150, "number_of_fragments" : 3} }}')
             )$hits$hits
t3 <- result_asdf(t2)

tt <- Search_template_render(body = body2)

test_fx <- function(res, x) {
  res_ls = list()
  for(i in x) {
    spp_tmp <- paste(res[[i]]$`_source`$Scientific_Name, collapse = "; ")
    rest <- res[[i]]$`_source`[1:12]
    cur_dat <- data.frame(rest, species = spp_tmp)
    res_ls[[i]] <- cur_dat
  }
  res_df <- dplyr::bind_rows(res_ls)
  return(res_df)
}

test_hi <- function(res) {
  abbrev <- function(x) {
    if(length(x) > 3) {
      paste(x[1:3], collapse = "...")
    } else {
      paste(x, collapse = "...")
    }
  }
  res_ls = list()
  for(i in 1:length(res)) {
    hi_tmp <- lapply(res[[i]]$highlight, FUN = abbrev)
    hi_tmp <- str_replace_all(hi_tmp,
                              "[ ]{2,}|\n",
                              " ")
    res_ls[[i]] <- hi_tmp
  }
  res_df <- unlist(res_ls)
  return(res_df)
}

y <- test_fx(tt, 1:3)

o <- result_asdf(tt)
hi1 <- test_hi(tt)
hi2 <- get_highlight(tt)


