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

connect()
if(index_exists("esadocs")) {
  index_delete("esadocs")
}
index_create("esadocs", body = settings)

# cleanup
rm(list = c("analyzer_json", "fedreg_json", "fiveyr_json",
            "recplan_json", "consult_json", "policy_json",
            "candid_json", "consag_json", "misc_json"))

###############################################################################
# 2. Load esadocs dfs

src <- "~/Data/ESAdocs/rda"
fils <- list.files(src, full.names = TRUE)
res <- lapply(fils, load, envir=.GlobalEnv)
ls()


###############################################################################
# 3. A function to load the text to the data.frames

get_txt <- function(df) {
  read_txt <- function(f) { paste(readLines(f), collapse = "\n") }
  texts <- lapply(df$txt_path, read_txt)
  df$raw_txt <- unlist(texts)
  return(df)
}

subset_df <- function(df, type) {
  cur_path <- file.path("/home/jacobmalcom/Data/ESAdocs", type)
  type_fils <- list.files(cur_path, full.names = TRUE)
  subd <- dplyr::filter(df, pdf_path %in% type_fils)
  return(subd)
}

missing_df <- function(df, type) {
  cur_path <- file.path("/home/jacobmalcom/Data/ESAdocs", type)
  type_fils <- list.files(cur_path, full.names = TRUE)
  subd <- dplyr::filter(df, !(pdf_path %in% type_fils))
  return(subd)
}

load_to_es <- function(df, index = "esadocs", type) {
  df$pdf_path <- gsub(df$pdf_path,
                      pattern = "https://defend-esc-dev.org",
                      replacement = "/home/jacobmalcom/Data")
  df$txt_path <- gsub(df$txt_path,
                      pattern = "https://defend-esc-dev.org",
                      replacement = "/home/jacobmalcom/Data")
  df$txt_path <- gsub(df$txt_path,
                      pattern = "pdf$|PDF$",
                      replacement = "txt")
  sub <- subset_df(df, type)
  connect()
  brks <- seq(1, dim(sub)[1], 100)
  for(i in 1:length(brks)) {
    st <- brks[i]
    en <- ifelse(brks[i] + 99 < dim(sub)[1],
                 brks[i] + 99,
                 dim(sub)[1])
    cur_tst <- get_txt(sub[st:en, ])
    bulk <- docs_bulk(cur_tst, index = index, type = type)
    message(sprintf("Added records %s to %s\n", st, en))
  }
}

load_to_es(consag_elast, "esadocs", "conserv_agmt")
load_to_es(adddoc_elast, "esadocs", "federal_register")
load_to_es(fedreg_elast, "esadocs", "federal_register")
load_to_es(consult_elast, "esadocs", "consultation")
load_to_es(recplan_elast, "esadocs", "recovery_plan")
load_to_es(crithab_elast, "esadocs", "federal_register")
load_to_es(fiveyr_elast, "esadocs", "five_year_review")



