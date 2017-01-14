# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(elastic)
library(esadocs)
library(jsonlite)
library(pdftools)
library(tools)

###############################################################################
# 1. Set up elastic indices

analyzer_json <- load_es_json("inst/extdata/esadocs_analyzer.json")
candid_json <- load_es_json("inst/extdata/candidate_mapping.json")
consag_json <- load_es_json("inst/extdata/conserv_agmt_mapping.json")
consult_json <- load_es_json("inst/extdata/consultation_mapping.json")
fedreg_json <- load_es_json("inst/extdata/federal_register_mapping.json")
fiveyr_json <- load_es_json("inst/extdata/five_year_review_mapping.json")
misc_json <-  load_es_json("inst/extdata/misc_mapping.json")
policy_json <-  load_es_json("inst/extdata/policy_mapping.json")
recplan_json <- esadocs::load_es_json("inst/extdata/recovery_plan_mapping.json")

settings <- make_es_settings(analyzer = c(analyzer_json),
                             mappings = c(
                               candid_json,
                               consag_json,
                               consult_json,
                               fedreg_json,
                               fiveyr_json,
                               misc_json,
                               policy_json,
                               recplan_json
                             ))

# settings <- make_es_settings(analyzer = c(analyzer_json),
#                              mappings = c(candid_json))

connect()
if(index_exists("esadocs")) {
  index_delete("esadocs")
}
index_create("esadocs", body = settings)

# cleanup
rm(list = c("analyzer_json", "fedreg_json", "fiveyr_json", "recplan_json", "consult_json"))

###############################################################################
# 2. Load and prep the data from .rda

###########!!!!!!!!!!!!!!!!!!!!!!##############
# ALL OF THIS IS OBSOLETE; USE THE SCRIPT
# make_dfs4elastic.R
###########!!!!!!!!!!!!!!!!!!!!!!##############

# BASED <- "~/ESAdocs"
# load(file.path(BASED, "rda", "ECOS_species_links_2016-12-09.rda"))
# load(file.path(BASED, "rda", "TECP_table_2016-12-09.rda"))

# loads fedreg_table, addoc_table, CCAA_table, CCA_table, HCP_table, SHA_table,
# crithab_table, fiveyr_table, species_table:
# load(file.path(BASED, "rda", "ECOS_species_tables_2016-12-09.rda"))

# data("ecos_doc_links")
# data("TECP_domestic")
# data("federal_register_table")
# data("recovery_plan_table")
# data("five_year_review_table")

# ecos_doc_links <- filter(ECOS_species_links,
#                          Scientific_Name %in% unique(TECP_table$Scientific_Name))
# fedreg <- filter(ecos_doc_links, type == "federal_register")
# recpln <- filter(ecos_doc_links, type == "recovery_plan")
# fiveyr <- filter(ecos_doc_links, type == "five_year_review")
#
# names(fedreg) <- c("species", "Doc_Link", "link", "text", "type")
# names(recpln) <- c("species", "Doc_Link", "link", "text", "type")
# names(fiveyr) <- c("species", "Doc_Link", "link", "text", "type")
#
# fr_dat <- bulk_fedreg_prep(fedreg, federal_register_table)
# rp_dat <- bulk_recplan_prep(recpln, recovery_plan_table)
# fy_dat <- bulk_fiveyr_prep(fiveyr, five_year_review_table)

###############################################################################
# 2b. Load the prepped data_frames
BASED <- "~/Work/Data/esadocs/rda"
flist <- list.files(BASED, pattern = "elast.rda", full.names = TRUE)
load_res <- lapply(flist, load)

###############################################################################
# 3. Load the prepped data into elastic indices

connect()
chunked_es_loading(fedreg_elast, index = "esadocs", type = "federal_register")
chunked_es_loading(recplan_elast, index = "esadocs", type = "recovery_plan")
chunked_es_loading(fiveyr_elast, index = "esadocs", type = "five_year_review")
chunked_es_loading(adddoc_elast, index = "esadocs", type = "federal_register")
chunked_es_loading(consag_elast, index = "esadocs", type = "conserv_agmt")
chunked_es_loading(crithab_elast, index = "esadocs", type = "critical_habitat")
chunked_es_loading(consult_elast, index = "esadocs", type = "consultation")

cons_fils <- list.files("~/Work/Data/esadocs/pdfs/consultation",
                        full.names = TRUE)
cons_name <- basename(cons_fils)
get_text <- function(f) {
  txt <- paste(pdf_text(f), collapse = "\n")
  return(txt)
}
cons_rawt <- lapply(cons_fils, get_text)

data_fils <- list.files("~/Work/Data/esadocs/rda", full.names = TRUE)
load(data_fils[4])

cons_subs <- filter(consult_elast, file_name %in% cons_name)
cons_subs$raw_txt <- cons_rawt

# Now for an on-server test...
cons_fils <- list.files("~/Data/ESAdocs/consultation", full.names = TRUE)
cons_name <- basename(cons_fils)
cons_text <- gsub(cons_fils, pattern = "ESAdocs", replacement = "ESAdocs_text")
cons_text <- gsub(cons_text, pattern = "pdf$|PDF$", replacement = "txt")
get_text <- function(f) {
  txt <- paste(readLines(f), collapse = "\n")
  return(txt)
}
tmp <- lapply(head(cons_text), get_text)
adf <- data.frame(raw_txt = unlist(tmp),
                  pdf_path = head(cons_fils),
                  file_name = head(cons_name),
                  txt_path =  head(cons_text),
                  stringsAsFactors = FALSE)
adf$pdf_url <- gsub(adf$pdf_path,
                    pattern = "/home/jacobmalcom/Data/",
                    replacement = "https://esadocs.cci-dev.org/")

data_fils <- list.files("~/Data/ESAdocs/rda", full.names = TRUE)
load(data_fils[3])
consult_elast$pdf_path <- gsub(consult_elast$pdf_path,
                               pattern = "defend-esc-dev",
                               replacement = "esadocs.cci-dev")

names(consult_elast)
tmp <- left_join(adf, consult_elast, by = c("pdf_url" = "pdf_path"))
tmp$pdf_path <- tmp$pdf_url
tmp <- select(tmp, -raw_txt.y, -file_name.y, -txt_path.y, -pdf_url)
names(tmp)[1] <- "raw_txt"
names(tmp)[3] <- "file_name"
names(tmp)[4] <- "txt_path"
res <- docs_bulk(tmp, index = "esadocs", type = "consultation")

# checks
stats <- index_stats("esadocs")
sprintf("%s documents", stats$indices$esadocs$total$docs$count)
sprintf("%s bytes", stats$indices$esadocs$total$store$size_in_bytes)

