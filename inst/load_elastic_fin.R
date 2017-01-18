# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(elastic)
library(esadocs)
library(jsonlite)
library(pdftools)
library(pdftext)
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
  type_fils <- list.files(cur_path)
  subd <- dplyr::filter(df, !(file_name %in% type_fils))
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
  sub$pdf_path <- gsub(sub$pdf_path,
                       pattern = "/home/jacobmalcom/Data",
                       replacement = "https://esadocs.cci-dev.org")
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

load_to_es(consag_elast, "esadocs", "conserv_agmt")       # 1062  # 1080
load_to_es(adddoc_elast, "esadocs", "federal_register")   # 508   # 546
load_to_es(fedreg_elast, "esadocs", "federal_register")   # 3340  # 3483
load_to_es(crithab_elast, "esadocs", "federal_register")  # 696   # 731
load_to_es(consult_elast, "esadocs", "consultation")      # 3371  # 3471
load_to_es(recplan_elast, "esadocs", "recovery_plan")     # 486   # 654
load_to_es(fiveyr_elast, "esadocs", "five_year_review")   # 1221  # 1286
load_to_es(policy_elast, "esadocs", "policy")             # 23    # 23
load_to_es(misc_elast, "esadocs", "misc")                 # 3191  # 3222
load_to_es(candidate_elast, "esadocs", "candidate")       # 96    # 96

###############################################################################
# OK, now I need to ID the files that were missed, send that over to Azure,
# find the files, send them back here, check for text | OCR, and load. Whew.
consag_miss <- missing_df(consag_elast, "conserv_agmt")
adddoc_miss <- missing_df(adddoc_elast, "federal_register")
fedreg_miss <- missing_df(fedreg_elast, "federal_register")
crithab_miss <- missing_df(crithab_elast, "federal_register")
consult_miss <- missing_df(consult_elast, "consultation")
recplan_miss <- missing_df(recplan_elast, "recovery_plan")
fiveyr_miss <- missing_df(fiveyr_elast, "five_year_review")
misc_miss <- missing_df(misc_elast, "misc")

missing_docs <- save(consag_miss,
                     adddoc_miss,
                     fedreg_miss,
                     crithab_miss,
                     consult_miss,
                     recplan_miss,
                     fiveyr_miss,
                     misc_miss,
                     file = "~/Data/ESAdocs/rda/missing_init_ES_load.rda")

###############################################################################
# This section is run on Azure...
load("/datadrive/data/ESAdocs/missing_init_ES_load.rda")

find_missing <- function(df) {
  fils_here <- unlist(lapply(df$file_name, find_patt))
  fils_info <- file.info(fils_here)
  fils_info$path <- row.names(fils_info)
  row.names(fils_info) <- seq(1, length(fils_info[,1]))
  fils_info$file_name <- basename(fils_info$path)
  fils_uniq <- dplyr::distinct(fils_info, size, file_name, .keep_all = TRUE)
  fils_keep <- get_biggest(fils_uniq)
  return(fils_keep)
}

find_patt <- function(f) {
  init <- list.files(
            "/datadrive/data",
            pattern = paste0(f, "$"),
            full.names=TRUE,
            recursive = TRUE)
  filt <- init[grep(init, pattern = paste0("/", f, "$"))]
  return(filt)
}

get_biggest <- function(df) {
  tmp <- aggregate(df$size ~ df$file_name, FUN = max)
  names(tmp) <- c("file_name", "size")
  dplyr::left_join(tmp, df)
}

adddoc_here <- find_missing(adddoc_miss)
consag_here <- find_missing(consag_miss)
consult_here <- find_missing(consult_miss)
crithab_here <- find_missing(crithab_miss)
fedreg_here <- find_missing(fedreg_miss)
fiveyr_here <- find_missing(fiveyr_miss)
misc_here <- find_missing(misc_miss)
recplan_here <- find_missing(recplan_miss)

dim(adddoc_here)
dim(consag_here)
dim(consult_here)
dim(crithab_here)
dim(fedreg_here)
dim(fiveyr_here)
dim(misc_here)
dim(recplan_here)

###############################################################################

###############################################################################
# This section is run on Azure, sending files to GCE
#
# First need to OCR/extract all docs, then move to GCE
all_fils <- c(adddoc_here$path,
              consag_here$path,
              consult_here$path,
              crithab_here$path,
              fedreg_here$path,
              fiveyr_here$path,
              misc_here$path,
              recplan_here$path)
all_fils <- unique(all_fils)

wrap_ocrmypdf <- function(infil, outdir) {
  base_dir <- dirname(infil)
  base_fil <- basename(infil)
  nospace <- gsub(x = base_fil, pattern = " ", replacement = "_")
  nospace <- gsub(x = nospace, pattern = "&", replacement = "and")
  nospace <- gsub(x = nospace, pattern = "\\(|\\)", replacement = "")
  nospace <- gsub(x = nospace, pattern = "\\,", replacement = "")
  nospace <- file.path(base_dir, nospace)
  file.rename(infil, nospace)
  outf <- file.path(outdir, basename(nospace))
  cmd <- paste0("ocrmypdf ",
                "--deskew ",
                "--rotate-pages --rotate-pages-threshold 10 ",
                "--oversample 500 ",
                "--skip-text ",
                "-l eng --tesseract-config ~/asciimostly '",
                nospace,
                "' ",
                outf)
  print(paste("Checking", nospace))
  embedded <- try(check_embed(nospace))
  if(class(embedded) != "try-error") {
    if(embedded) {
      # print(sprintf("\n\tCopying %s; writing to %s...\n", nospace, outf))
      file.copy(nospace, outf)
    } else {
      if(!file.exists(outf)) {
        # print(sprintf("\n\tProcessing %s; writing to %s...\n", nospace, outf))
        res <- try(system(command = cmd, intern = FALSE, wait = TRUE))
        if(res[1] == "try-error") {
          message(paste("\n\tSomething went wrong:\n\n", res))
          message(paste("\n\tFile:\t", nospace))
        }
      }
    }
  } else {
    print(paste("Error with file text embed check:", nospace))
  }
}

check_embed <- function(file) {
  text <- pdftools::pdf_text(file)
  nchr <- unlist(lapply(text, nchar))
  if(mean(nchr, na.rm = TRUE) > 100) {
    temp <- pdftools::pdf_info(file)
    if (length(temp) > 1) {
      info <- list(temp)
    }
    else {
      info <- temp
    }
    if (!is.atomic(info[[1]][1]) & !is.na(info[[1]][1])) {
      if (!is.null(info[[1]]$keys)) {
        if (!is.null(info[[1]]$keys$Producer)) {
          if (grepl(info[[1]]$keys$Producer, pattern = "Distiller|Word|Library|Ghost|Acrobat Pro|Adobe Acrobat")) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }
  return(FALSE)
}

run_wrapper <- function(search_dir, write_dir) {
  cur_files <- list.files(search_dir,
                          pattern = "*.pdf",
                          ignore.case = TRUE,
                          recursive = TRUE,
                          full.names = TRUE)
  if(!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }
  cur_res <- parallel::mclapply(X = cur_files,
                                FUN = wrap_ocrmypdf,
                                outdir = write_dir,
                                mc.preschedule = FALSE,
                                mc.cores = 14)
}

ocr_wrap <- function(fs, write_dir) {
  if(!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }
  cur_res <- parallel::mclapply(X = fs,
                                FUN = wrap_ocrmypdf,
                                outdir = write_dir,
                                mc.preschedule = FALSE,
                                mc.cores = 14)
}

adddoc_ocr <- ocr_wrap(adddoc_here$path,
                       "/datadrive/data/ESAdocs_missed/federal_register")
fedreg_ocr <- ocr_wrap(fedreg_here$path,
                       "/datadrive/data/ESAdocs_missed/federal_register")
crithab_ocr <- ocr_wrap(crithab_here$path,
                       "/datadrive/data/ESAdocs_missed/federal_register")
consult_ocr <- ocr_wrap(consult_here$path,
                       "/datadrive/data/ESAdocs_missed/consultation")
consag_ocr <- ocr_wrap(consag_here$path,
                       "/datadrive/data/ESAdocs_missed/conserv_agmt")
recplan_ocr <- ocr_wrap(recplan_here$path,
                       "/datadrive/data/ESAdocs_missed/recovery_plan")
misc_ocr <- ocr_wrap(misc_here$path,
                       "/datadrive/data/ESAdocs_missed/misc")
fiveyr_ocr <- ocr_wrap(fiveyr_here$path,
                       "/datadrive/data/ESAdocs_missed/five_year_review")

rsync_away <- function(f, d) {
  esc_f <- gsub(f, pattern = " ", replacement = "\\ ", fixed = TRUE)
  esc_f <- gsub(esc_f, pattern = "'", replacement = "\\'", fixed = TRUE)
  cmd <- paste("rsync -vc",
               esc_f,
               paste0("jacobmalcom@35.185.10.154:",
                      "/home/jacobmalcom/Data/ESAdocs_miss/",
                      d))
  system(cmd)
}

res <- lapply(adddoc_here$path, rsync_away, d = "federal_register")
length(res[res > 0])
res <- lapply(consag_here$path, rsync_away, d = "conserv_agmt")
length(res[res > 0])
res <- lapply(consult_here$path, rsync_away, d = "consultation")
length(res[res > 0])
res <- lapply(crithab_here$path, rsync_away, d = "federal_register")
length(res[res > 0])
res <- lapply(fedreg_here$path, rsync_away, d = "federal_register")
length(res[res > 0])
res <- lapply(fiveyr_here$path, rsync_away, d = "five_year_review")
length(res[res > 0])
res <- lapply(misc_here$path, rsync_away, d = "misc")
length(res[res > 0])
res <- lapply(recplan_here$path, rsync_away, d = "recovery_plan")
length(res[res > 0])

###############################################################################

###############################################################################
# Now, back over to GCE
fils_list <- list.files("~/Data/ESAdocs_miss",
                        full.names = TRUE,
                        recursive = TRUE)

# check_text <- function(x) {
#   txt <- pdf_text(x)
#   nch <- unlist(lapply(txt, nchar))
#   print(mean(nch), na.rm = TRUE)
#   print(length(nch))
#   if(mean(nch, na.rm = TRUE) > 100) return(TRUE)
#   return(FALSE)
# }
#
# with_text <- unlist(lapply(fils_list[1:5], check_text))
# Bunch of files without a text layer, so need to grab the OCRmyPDF code...

ocr_res_17Jan <- run_wrapper("~/Data/ESAdocs_miss", "~/Data/ESAdocs_miss_OCR")
