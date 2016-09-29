# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(elastic)
library(jsonlite)
library(tools)

###############################################################################
# 1. Set up elastic indices

analyzer <- load_es_json("inst/extdata/esadocs_analyzer.json")
fedreg <-  load_es_json("inst/extdata/federal_register_mapping.json")
fiveyr <-  load_es_json("inst/extdata/five_year_review_mapping.json")
recplan <- load_es_json("inst/extdata/recovery_plan_mapping.json")

settings <- make_es_settings(analyzer = c(analyzer),
                             mappings = c(
                               fedreg,
                               fiveyr,
                               recplan
                             ))
connect()
index_delete("esadocs")
index_create("esadocs", body = settings)



###############################################################################
# 2. Load and prep the data from .rda








###############################################################################
# 3. Load the prepped data into elastic indices
