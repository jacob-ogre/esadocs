# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(elastic)
library(jsonlite)
library(tools)

###############################################################################
# 1. Set up elastic indices

analyzer <- paste(readLines("data-raw/esadocs_analyzer.json"), collapse = "\n")

fedreg <-  paste(readLines("data-raw/federal_register_mapping.json"),
                 collapse = "\n")

fiveyr <-  paste(readLines("data-raw/five_year_review_mapping.json"),
                          collapse = "\n")

recplan <-  paste(readLines("data-raw/recovery_plan_mapping.json"),
                           collapse = "\n")

settings <- make_es_settings(analyzer = list(analyzer),
                             mappings = list(
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
