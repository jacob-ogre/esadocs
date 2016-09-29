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

analyzer <- load_es_json("inst/extdata/esadocs_analyzer.json")
fedreg <-  load_es_json("inst/extdata/federal_register_mapping.json")
fiveyr <-  load_es_json("inst/extdata/five_year_review_mapping.json")
recplan <- load_es_json("inst/extdata/recovery_plan_mapping.json")
s7a2 <- load_es_json("inst/extdata/consultation_mapping.json")

settings <- make_es_settings(analyzer = c(analyzer),
                             mappings = c(
                               fedreg,
                               fiveyr,
                               recplan,
                               s7a2
                             ))
connect()
index_delete("esadocs")
index_create("esadocs", body = settings)
index_settings()

# cleanup
rm(list = c("analyzer", "fedreg", "fiveyr", "recplan", "s7a2"))

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
tmp <- bulk_ecosdocs_prep(fedreg, federal_register_table, "federal_register")


###############################################################################
# 3. Load the prepped data into elastic indices
