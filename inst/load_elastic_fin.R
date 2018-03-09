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
settings <- make_es_settings(analyzer = c(analyzer_json),
                             mappings = c(
                               candid_json,
                               consag_json
                             )
)

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
  read_txt <- function(f) {
    res <- try(paste(readLines(f), collapse = "\n"))
    if(class(res) == "try-error") res <- ""
    res
  }
  texts <- lapply(df$txt_path, read_txt)
  df$raw_txt <- unlist(texts)
  return(df)
}

subset_df <- function(df, type) {
  cur_path <- file.path("/home/cci/Data/ESAdocs/text", type)
  print(cur_path)
  type_fils <- list.files(cur_path, full.names = TRUE)
  print(length(type_fils))
  subd <- dplyr::filter(df, pdf_path %in% type_fils)
  return(subd)
}

load_to_es <- function(df, index = "esadocs", type) {
  df$pdf_path <- gsub(df$pdf_path,
                      pattern = "https://defend-esc-dev.org",
                      replacement = "/home/cci/Data")
  df$txt_path <- gsub(df$txt_path,
                      pattern = "https://defend-esc-dev.org/ESAdocs_text",
                      replacement = "/home/cci/Data/ESAdocs/text")
  df$txt_path <- gsub(df$txt_path,
                      pattern = "/home/jacobmalcom/Data/ESAdocs_text",
                      replacement = "/home/cci/Data/ESAdocs/text")
  df$txt_path <- gsub(df$txt_path,
                      pattern = "pdf$|PDF$",
                      replacement = "txt")
  # sub <- subset_df(df, type)
  sub <- df
  # print(dim(sub))
  sub$pdf_path <- gsub(sub$pdf_path,
                       pattern = "/home/cci/Data",
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

missing_df <- function(df, type) {
  cur_path <- file.path("/home/jacobmalcom/Data/ESAdocs", type)
  type_fils <- list.files(cur_path)
  subd <- dplyr::filter(df, !(file_name %in% type_fils))
  return(subd)
}

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

save(adddoc_here,
     consag_here,
     consult_here,
     crithab_here,
     fedreg_here,
     fiveyr_here,
     misc_here,
     recplan_here, file = "/datadrive/data/ESAdocs_miss/fils_here.rda")

load("/datadrive/data/ESAdocs_miss/fils_here.rda")

wrap_ocrmypdf <- function(infil, outdir) {
  base_dir <- dirname(infil)
  base_fil <- basename(infil)
  encrypt <- try(pdf_info(infil)$encrypted, silent = TRUE)
  if(class(encrypt) != "try-error") {
    if(encrypt) {
      res <- try_pdftk_cast(infil)
      if(res == 0) {
        infil <- file.path("/datadrive/data/ESAdocs_miss/temp_encr", base_fil)
        base_dir <- dirname(infil)
        base_fil <- basename(infil)
      } else {
        res <- try_ppm_cast(infil)
        if(grepl(res, pattern = "Error")) {
          return(paste("Encrypted:", infil))
        } else {
          infil <- res$infil
          base_dir <- res$base_dir
          base_fil <- res$base_fil
        }
      }
    }
  } else {
    return(paste("Not a pdf:", infil))
  }
  nospace <- gsub(x = base_fil, pattern = " ", replacement = "_")
  nospace <- gsub(x = nospace, pattern = "&", replacement = "and")
  nospace <- gsub(x = nospace, pattern = "\\(|\\)|\'|\"", replacement = "")
  nospace <- gsub(x = nospace, pattern = "\\,", replacement = "")
  nospace <- file.path(base_dir, nospace)
  file.rename(infil, nospace)
  outf <- file.path(outdir, basename(nospace))
  cmd <- paste0("ocrmypdf ",
                "--deskew ",
                "--rotate-pages --rotate-pages-threshold 10 ",
                "--oversample 300 ",
                "--skip-text ",
                "-l eng --tesseract-config ~/asciimostly '",
                nospace,
                "' ",
                outf)
  embedded <- try(check_embed(nospace), silent = TRUE)
  if(class(embedded) != "try-error") {
    if(embedded) {
      file.copy(nospace, outf)
      return("File copied")
    } else {
      if(!file.exists(outf)) {
        res <- try(system(command = cmd, intern = FALSE, wait = TRUE))
        if(res == 15 | res == 8) {
          res <- try_ppm_cast(nospace)
          if(grepl(res, pattern = "Error")) {
            return(res)
          } else {
            nospace <- res$infil
            cmd <- paste0("ocrmypdf ",
                          "--deskew ",
                          "--rotate-pages --rotate-pages-threshold 10 ",
                          "--oversample 300 ",
                          "--skip-text ",
                          "-l eng --tesseract-config ~/asciimostly '",
                          nospace,
                          "' ",
                          outf)
            res <- try(system(command = cmd, intern = FALSE, wait = TRUE))
            if(res == 0) {
              return(paste("OCR'd, maybe:", outf))
            }
            return(paste("OCR error:", res))
          }
          return(paste("OCR error:", res))
        }
        return(paste("OCR'd, maybe:", outf))
      }
      return("File exists")
    }
  } else {
    return(paste("Text embed error:", infil))
  }
}

try_pdftk_cast <- function(infil) {
  base_dir <- dirname(infil)
  base_fil <- basename(infil)
  tmp_fil <- file.path("/datadrive/data/ESAdocs_miss/temp_encr", base_fil)
  cmd <- paste("pdftk",
               infil,
               "cat output",
               tmp_fil)
  res <- try(system(cmd, intern = FALSE, wait = TRUE))
  return(res)
}

try_ppm_cast <- function(infil) {
  base_fil <- basename(infil)
  ppmf <- file.path("/datadrive/data/ESAdocs_miss/temp_encr",
                    paste0(base_fil, ".png"))
  cmd <- paste("pdftoppm -r 300 -png", infil, ">", ppmf)
  res <- try(system(cmd, intern = FALSE, wait = TRUE))
  if(res == 0) {
    pdff <- paste0(ppmf, ".pdf")
    cmd <- paste("convert", ppmf, pdff)
    res <- try(system(cmd, intern = FALSE, wait = TRUE))
    if(res == 0) {
      infil <- pdff
      base_dir <- dirname(pdff)
      base_fil <- basename(pdff)
      unlink(ppmf)
      return(list(infil = infil,
                  base_dir = base_dir,
                  base_fil = base_fil))
    } else {
      return(paste("Error @convert:", infil))
    }
  } else {
    return(paste("Error @pdftoppm:", infil))
  }
}

check_embed <- function(file) {
  text <- pdftools::pdf_text(file)
  nchr <- unlist(lapply(text, nchar))
  if(mean(nchr, na.rm = TRUE) > 100) {
    return(TRUE)
  }
  return(FALSE)
}

ocr_wrap <- function(fs, write_dir) {
  if(!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }
  cur_res <- parallel::mclapply(X = fs,
                                FUN = wrap_ocrmypdf,
                                outdir = write_dir,
                                mc.preschedule = FALSE,
                                mc.cores = 13)
}

adddoc_ocr <- ocr_wrap(adddoc_here$path,
                       "/datadrive/data/ESAdocs_miss/federal_register")
fedreg_ocr <- ocr_wrap(fedreg_here$path,
                       "/datadrive/data/ESAdocs_miss/federal_register")
crithab_ocr <- ocr_wrap(crithab_here$path,
                       "/datadrive/data/ESAdocs_miss/federal_register")
consult_ocr <- ocr_wrap(consult_here$path,
                       "/datadrive/data/ESAdocs_miss/consultation")
consag_ocr <- ocr_wrap(consag_here$path,
                       "/datadrive/data/ESAdocs_miss/conserv_agmt")
recplan_ocr <- ocr_wrap(recplan_here$path,
                       "/datadrive/data/ESAdocs_miss/recovery_plan")
misc_ocr <- ocr_wrap(misc_here$path,
                     "/datadrive/data/ESAdocs_miss/misc")
fiveyr_ocr <- ocr_wrap(fiveyr_here$path,
                       "/datadrive/data/ESAdocs_miss/five_year_review")

# Use `scp` to copy the files over to GCE
###############################################################################

###############################################################################
# Now, back over to GCE

get_txt <- function(df, type) {
  temp_pdf <- file.path("/home/jacobmalcom/Data/ESAdocs_miss",
                        type,
                        df$file_name)
  temp_pdf <- gsub(x = temp_pdf, pattern = " ", replacement = "_")
  temp_pdf <- gsub(x = temp_pdf, pattern = "&", replacement = "and")
  temp_pdf <- gsub(x = temp_pdf, pattern = "\\(|\\)|\'|\"", replacement = "")
  temp_pdf <- gsub(x = temp_pdf, pattern = "\\,", replacement = "")
  read_txt <- function(f) {
    if(!file.exists(f)) {
      alt_f <- paste0(f, ".png.pdf")
      if(!file.exists(alt_f)) {
        warning(paste(f, "or", alt_f, "does not exist..."))
        return("")
      } else {
        f <- alt_f
      }
    }
    res <- try(paste(pdftools::pdf_text(f), collapse = "\n"))
    if(class(res) == "try-error") {
      warning(res)
      return("")
    }
    return(res)
  }
  texts <- lapply(temp_pdf, read_txt)
  missing_pdfs <- temp_pdf[!file.exists(temp_pdf) &
                             !file.exists(paste0(temp_pdf, ".png.pdf"))]
  df$raw_txt <- unlist(texts)
  df$pdf_path_2 <- temp_pdf
  return(list(df = df, missing_pdfs = missing_pdfs))
}

load_miss_es <- function(df, index = "esadocs", type) {
  sub <- df
  connect()
  brks <- seq(1, dim(sub)[1], 100)
  missing_pdfs <- c()
  for(i in 1:length(brks)) {
    st <- brks[i]
    en <- ifelse(brks[i] + 99 < dim(sub)[1],
                 brks[i] + 99,
                 dim(sub)[1])
    result <- get_txt(sub[st:en, ], type)
    cur_tst <- result$df
    missing_pdfs <- c(missing_pdfs, result$missing_pdfs)
    src_path <- cur_tst$pdf_path_2
    cur_tst$pdf_path <- file.path("/home/jacobmalcom/Data/ESAdocs",
                                  type,
                                  cur_tst$file_name)
    bulk <- docs_bulk(cur_tst, index = index, type = type)
    message(sprintf("Added records %s to %s\n", st, en))
  }
  return(missing_pdfs)
}

adddoc_test <- load_miss_es(adddoc_miss, "esadocs", "federal_register")
consag_test <- load_miss_es(consag_miss, "esadocs", "conserv_agmt")
consult_test <- load_miss_es(consult_miss, "esadocs", "consultation")
recplan_test <- load_miss_es(recplan_miss, "esadocs", "recovery_plan")
crithab_test <- load_miss_es(crithab_miss, "esadocs", "federal_register")
fiveyr_test <- load_miss_es(fiveyr_miss, "esadocs", "five_year_review")
misc_test <- load_miss_es(misc_miss, "esadocs", "misc")

save(adddoc_test, consag_test, consult_test, recplan_test, crithab_test,
     fiveyr_test, misc_test, file = "~/Data/ESAdocs/missed_files.rda")

#############################################################################
# Fix PDF paths
#
# I messed up a bunch of paths to PDFs when loading elasticsearch, so need to
# fix them. Plus, ca. 400 missing PDFs were added while in a different directory
# and need to be moved. Last, this is actually useful for learning how to use
# the update API, which we will want to use extensively (e.g., tagging, NLP)
#
# The first is a test using my local ES with just 8 docs. This works:
res <- Search("esadocs", stored_fields = "pdf_path", q = "consultation")
idx <- sapply(1:length(res$hits$hits), function(x) res$hits$hits[[x]]$`_id`)

getted <- docs_mget("esadocs", type = "consultation", ids = idx)
cur <- sapply(1:length(getted$docs),
              function(x) getted$docs[[x]]$`_source`$pdf_path)
new <- gsub(cur, pattern = "defend-esc-dev", replacement = "cci-dev")
# upd <- docs_update(
#   index = "esadocs",
#   type = "consultation",
#   id = "AVmX25rutEhH8-nPbY4h",
#   body = list(doc = list(pdf_path = new))
# )

update_paths <- function(type, id, new_path) {
  result <- docs_update(
    index = "esadocs",
    type = type,
    id = id,
    body = list(doc = list(pdf_path = new_path))
  )
  return(result$result)
}

ares <- purrr::pmap(.l = list(type="consultation",
                              id = idx,
                              new_path = new),
                    .f = update_paths)

##############################################################################
# Now, let's test a query on the remote...

res <- Search("esadocs",
              type = "misc",
              stored_fields = "pdf_path",
              size = 10000,
              q = "*")
idx <- sapply(1:length(res$hits$hits), function(x) res$hits$hits[[x]]$`_id`)
getted <- docs_mget("esadocs", type = "_all", ids = idx)
cur <- sapply(1:length(getted$docs),
              function(x) getted$docs[[x]]$`_source`$pdf_path)
new <- gsub(cur, pattern = "/home/jacobmalcom/Data", replacement = "")

a_res <- purrr::pmap(.l = list(type="misc",
                               id = idx,
                               new_path = new),
                     .f = update_paths)

fix_paths <- function(type) {

  update_paths <- function(type, id, new_path) {
    result <- docs_update(
      index = "esadocs",
      type = type,
      id = id,
      body = list(doc = list(pdf_path = new_path))
    )
    return(result$result)
  }

  res <- Search("esadocs",
                type = type,
                stored_fields = "pdf_path",
                size = 10000,
                q = "*")
  idx <- sapply(1:length(res$hits$hits), function(x) res$hits$hits[[x]]$`_id`)
  getted <- docs_mget("esadocs", type = "_all", ids = idx)
  cur <- sapply(1:length(getted$docs),
                function(x) getted$docs[[x]]$`_source`$pdf_path)
  new <- gsub(cur, pattern = "/home/jacobmalcom/Data", replacement = "")

  a_res <- purrr::pmap(.l = list(type = type,
                                 id = idx,
                                 new_path = new),
                       .f = update_paths)
  return(unlist(a_res))
}

policy_fix <- fix_paths("policy")
candidate_fix <- fix_paths("candidate")
consag_fix <- fix_paths("conserv_agmt")
table(consag_fix)
consultation_fix <- fix_paths("consultation")
fedreg_fix <- fix_paths("federal_register")
fiveyr_fix <- fix_paths("five_year_review")
misc_fix <- fix_paths("misc")
recplan_fix <- fix_paths("recovery_plan")


'
drwxrwxr-x 2 jacobmalcom jacobmalcom   4096 Jan 13 17:15 candidate
drwxrwxr-x 2 jacobmalcom jacobmalcom  57344 Jan 24 15:02 conserv_agmt
drwxrwxr-x 2 jacobmalcom jacobmalcom 253952 Jan 24 15:05 consultation
drwxrwxr-x 2 jacobmalcom jacobmalcom 221184 Jan 24 15:19 federal_register
drwxrwxr-x 2 jacobmalcom jacobmalcom  36864 Jan 24 15:19 five_year_review
drwxrwxr-x 2 jacobmalcom jacobmalcom 159744 Jan 24 15:19 misc
-rw-rw-r-- 1 jacobmalcom jacobmalcom   1279 Jan 24 12:07 missed_files.rda
drwxrwxr-x 2 jacobmalcom jacobmalcom   4096 Jan 13 18:21 policy
drwxrwxr-x 2 jacobmalcom jacobmalcom   4096 Jan 18 12:32 rda
drwxrwxr-x 2 jacobmalcom jacobmalcom  40960 Jan 24 15:20 recovery_plan'
