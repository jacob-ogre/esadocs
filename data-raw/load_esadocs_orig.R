# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(elastic)
library(esadocs)
library(jsonlite)
library(tools)

###############################################################################
# 1. Set up elastic indices

analyzer_json <- load_es_json("inst/extdata/esadocs_analyzer.json")
fedreg_json <-  load_es_json("inst/extdata/federal_register_mapping.json")
fiveyr_json <-  load_es_json("inst/extdata/five_year_review_mapping.json")
recplan_json <- load_es_json("inst/extdata/recovery_plan_mapping.json")
s7a2_json <- load_es_json("inst/extdata/consultation_mapping.json")

settings <- make_es_settings(analyzer = c(analyzer_json),
                             mappings = c(
                               fedreg_json,
                               fiveyr_json,
                               recplan_json,
                               s7a2_json
                             ))
connect()
index_delete("esadocs")
index_create("esadocs", body = settings)

# cleanup
rm(list = c("analyzer_json", "fedreg_json", "fiveyr_json", "recplan_json", "s7a2_json"))

###############################################################################
# 2. Load and prep the data from .rda

data("ecos_doc_links")
data("TECP_domestic")
data("federal_register_table")
data("recovery_plan_table")
data("five_year_review_table")

ecos_doc_links <- filter(ecos_doc_links,
                         Scientific_Name %in% unique(TECP_domestic$Scientific_Name))
fedreg <- filter(ecos_doc_links, type == "federal_register")
recpln <- filter(ecos_doc_links, type == "recovery_plan")
fiveyr <- filter(ecos_doc_links, type == "five_year_review")

names(fedreg) <- c("species", "Doc_Link", "link", "text", "type")
names(recpln) <- c("species", "Doc_Link", "link", "text", "type")
names(fiveyr) <- c("species", "Doc_Link", "link", "text", "type")

fr_dat <- bulk_fedreg_prep(fedreg, federal_register_table)
rp_dat <- bulk_recplan_prep(recpln, recovery_plan_table)
fy_dat <- bulk_fiveyr_prep(fiveyr, five_year_review_table)

###############################################################################
# 3. Load the prepped data into elastic indices

# brks <- seq(1, length(fr_dat[, 1]), 100)
# for(i in 1:length(brks)) {
#   if(brks[i] + 99 < length(fr_dat[,1])) {
#     cur_tst <- add_raw_txt(fr_dat[brks[i]:(brks[i] + 99), ])
#   } else {
#     cur_tst <- add_raw_txt(fr_dat[brks[i]:length(fr_dat[, 1]), ])
#   }
#   connect()
#   index_settings("esadocs")
#   bulk <- docs_bulk(cur_tst, index = "esadocs", type = "federal_register")
#   cat(sprintf("Added records %s to %s\n", brks[i], brks[i] + 99))
# }

chunked_es_loading(fr_dat, index = "esadocs", type = "federal_register")

brks <- seq(1, length(rp_dat[, 1]), 100)
for(i in 1:length(brks)) {
  if(brks[i] + 99 < length(rp_dat[,1])) {
    cur_tst <- add_raw_txt(rp_dat[brks[i]:(brks[i] + 99), ])
  } else {
    cur_tst <- add_raw_txt(rp_dat[brks[i]:length(rp_dat[, 1]), ])
  }
  connect()
  bulk <- docs_bulk(cur_tst, index = "esadocs", type = "recovery_plan")
  cat(sprintf("Added records %s to %s\n", brks[i], brks[i] + 99))
}

brks <- seq(1, length(fy_dat[, 1]), 100)
print(sprintf("%s documents to add\n", length(fy_dat[, 1])))
connect()
for(i in 1:length(brks)) {
  if(brks[i] + 99 < length(fy_dat[,1])) {
    cur_tst <- add_raw_txt(fy_dat[brks[i]:(brks[i] + 99), ])
  } else {
    cur_tst <- add_raw_txt(fy_dat[brks[i]:length(fy_dat[, 1]), ])
  }
  bulk <- docs_bulk(cur_tst, index = "esadocs", type = "five_year_review")
  cat(sprintf("Added records %s to %s\n", brks[i], brks[i] + 99))
}
