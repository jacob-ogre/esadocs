# BSD_2_clause

library(dplyr)
library(ecosscraper)
library(stringr)
library(tidyr)

# BASED <- "~/ESAdocs"
BASED <- "~/Work/Data/esadocs"
fils <- list.files(file.path(BASED, "rda"), recursive = TRUE, full.names = TRUE)
fils

# First, the raw links
load(fils[grep(fils, pattern = "species_links")])
pdfs <- filter(ECOS_species_links,
               grepl(ECOS_species_links$link, pattern = "pdf$|PDF$|gpo"))
head(pdfs)
length(unique(pdfs$link))
names(pdfs)
# [1] "Scientific_Name" "href"            "link"            "text"

# Next, load up the tables
load(fils[grep(fils, pattern = "species_tables")])
ls()
# "adddoc_table"       "CCAA_table"         "CCA_table"
# "crithab_table"      "fedreg_table"
# "fiveyr_table"       "HCP_table"          "pdfs_info"
# "recovery_table"     "SHA_table"          "species_table"

names(adddoc_table)
# "Date"          "Citation Page" "Title"         "Document Type" "Doc_Link"      "Species"

names(CCAA_table)
# "CCAA Plan Summaries" "Doc_Link"            "Species"

names(CCA_table)
# "CCA Plan Summaries" "Doc_Link"           "Species"

names(crithab_table)
# "Date"          "Citation Page" "Title"         "Document Type" "Status"        "Doc_Link"
# "Species"

names(fedreg_table)
# "Date"          "Citation Page" "Title"         "Doc_Link"      "Species"

names(fiveyr_table)
# "Date"     "Title"    "Doc_Link" "Species"

names(HCP_table)
# "HCP Plan Summaries" "Doc_Link"           "Species"

names(pdfs_info)
# "path"      "size"      "isdir"     "mode"      "mtime"     "ctime"     "atime"     "uid"
# "gid"       "uname"     "grname"    "MD5"       "file_name"

names(recovery_table)
# "Date"               "Title"              "Plan Action Status" "Plan Status"
# "Doc_Link"           "Species"

names(SHA_table)
# "SHA Plan Summaries" "Doc_Link"           "Species"

names(species_table)
# "Status"       "Date Listed"  "Lead Region"  "Where Listed" "Species"

###############################################################################
# Let's try some joins...
#
# First, Fed Reg docs
load(fils[grep(fils, pattern = "OCR_res")])
names(OCR_res)

fedreg_table$file <- basename(fedreg_table$Doc_Link)
fedreg_table$file <- ifelse(is.na(fedreg_table$file) | grepl(fedreg_table$file, pattern = "pdf$|PDF$"),
                            fedreg_table$file,
                            paste0(fedreg_table$file, ".pdf"))
fedreg_table$file <- clean_fname(fedreg_table$file)
OCR_res$file <- basename(OCR_res$infile)
fr_j1 <- left_join(fedreg_table, OCR_res, by = "file")
fr_j2 <- distinct(fr_j1, Doc_Link, Species, OCR_MD5, .keep_all = TRUE)
fr_j3 <- select(fr_j2, -error, -proc_time)
names(fr_j3)

fr_spp <- aggregate(Species ~ Doc_Link, data = fr_j3, FUN = unique)
fr_dat1 <- left_join(fr_spp, fr_j3, by = "Doc_Link")
spp_links <- distinct(fr_dat1, Species.x, .keep_all = TRUE)
spp_links <- filter(spp_links, Doc_Link != "http://ecos.fws.gov")
spp_links$local_pdf <- gsub(spp_links$outfile,
                            pattern = "/datadrive/data",
                            replacement = "https://defend-esc-dev.org")
spp_links$local_txt <- gsub(spp_links$local_pdf,
                            pattern = "ESAdocs",
                            replacement = "ESAdocs_text")
spp_links$local_txt <- gsub(spp_links$local_txt,
                            pattern = "pdf$|PDF$",
                            replacement = "txt")
fedreg_elast <- select(spp_links, -Species.y, -infile, -outfile)
fedreg_elast$Date <- as.Date(fedreg_elast$Date)
names(fedreg_elast) <- c("link", "species", "date", "fr_citation_page",
                         "title", "file_name", "pdf_md5", "n_pages",
                         "pdf_path", "txt_path")
fedreg_elast$type <- "federal_register"
fedreg_elast$raw_txt <- fedreg_elast$geo <- fedreg_elast$tags <- ""
save(fedreg_elast, file = "~/Work/Data/esadocs/rda/fedreg_elast.rda")

# Recovery plans
recovery_table$file <- basename(recovery_table$Doc_Link)
recovery_table$file <- ifelse(is.na(recovery_table$file) |
                                grepl(recovery_table$file, pattern = "pdf$|PDF$"),
                            recovery_table$file,
                            paste0(recovery_table$file, ".pdf"))
recovery_table$file <- clean_fname(recovery_table$file)
OCR_res$file <- basename(OCR_res$outfile)
rp_j1 <- left_join(recovery_table, OCR_res, by = "file")
rp_j2 <- distinct(rp_j1, Doc_Link, Species, OCR_MD5, .keep_all = TRUE)
rp_j3 <- select(rp_j2, -error, -proc_time)
names(rp_j3)

rp_spp <- aggregate(Species ~ Doc_Link, data = rp_j3, FUN = unique)
rp_dat1 <- left_join(rp_spp, rp_j3, by = "Doc_Link")
spp_links <- distinct(rp_dat1, Species.x, .keep_all = TRUE)
spp_links <- filter(spp_links, Doc_Link != "http://ecos.fws.gov")
spp_links$local_pdf <- gsub(spp_links$outfile,
                            pattern = "/datadrive/data",
                            replacement = "https://defend-esc-dev.org")
spp_links$local_txt <- gsub(spp_links$local_pdf,
                            pattern = "ESAdocs",
                            replacement = "ESAdocs_text")
spp_links$local_txt <- gsub(spp_links$local_txt,
                            pattern = "pdf$|PDF$",
                            replacement = "txt")
recplan_elast <- select(spp_links, -Species.y, -infile, -outfile)
recplan_elast$Date <- as.Date(recplan_elast$Date)
names(recplan_elast)
names(recplan_elast) <- c("link", "species", "date", "title",
                          "plan_act_status", "plan_status", "File_Base",
                          "pdf_md5", "n_pages", "pdf_path", "txt_path")
recplan_elast$type <- "recovery_plan"
recplan_elast$raw_txt <- recplan_elast$geo <- recplan_elast$tags <- ""
recplan_elast <- filter(recplan_elast,
                        !grepl(recplan_elast$pdf_path,
                               pattern = "federal_register"))
save(recplan_elast, file = "~/Work/Data/esadocs/rda/recplan_elast.rda")

# Critical Habitat
crithab_table$file <- basename(crithab_table$Doc_Link)
crithab_table$file <- ifelse(is.na(crithab_table$file) |
                               grepl(crithab_table$file, pattern = "pdf$|PDF$"),
                            crithab_table$file,
                            paste0(crithab_table$file, ".pdf"))
crithab_table$file <- clean_fname(crithab_table$file)
OCR_res$file <- basename(OCR_res$infile)
fr_j1 <- left_join(crithab_table, OCR_res, by = "file")
fr_j2 <- distinct(fr_j1, Doc_Link, Species, OCR_MD5, .keep_all = TRUE)
fr_j3 <- select(fr_j2, -error, -proc_time)
names(fr_j3)

fr_spp <- aggregate(Species ~ Doc_Link, data = fr_j3, FUN = unique)
fr_dat1 <- left_join(fr_spp, fr_j3, by = "Doc_Link")
spp_links <- filter(spp_links, Doc_Link != "http://ecos.fws.gov")
spp_links <- distinct(fr_dat1, Species.x, .keep_all = TRUE)
spp_links$local_pdf <- gsub(spp_links$outfile,
                            pattern = "/datadrive/data",
                            replacement = "https://defend-esc-dev.org")
spp_links$local_txt <- gsub(spp_links$local_pdf,
                            pattern = "ESAdocs",
                            replacement = "ESAdocs_text")
spp_links$local_txt <- gsub(spp_links$local_txt,
                            pattern = "pdf$|PDF$",
                            replacement = "txt")
crithab_elast <- select(spp_links, -Species.y, -infile, -outfile)
crithab_elast$Date <- as.Date(crithab_elast$Date)
names(crithab_elast)
names(crithab_elast) <- c("link", "species", "date", "fr_citation_page",
                          "title", "doc_type", "ch_status", "file_name",
                          "pdf_md5", "n_pages", "pdf_path", "txt_path")
crithab_elast$type <- "critical_habitat"
crithab_elast$raw_txt <- crithab_elast$geo <- crithab_elast$tags <- ""
save(crithab_elast, file = "~/Work/Data/esadocs/rda/crithab_elast.rda")

# Additional FR docs
adddoc_table <- distinct(adddoc_table, Doc_Link, Species, .keep_all = TRUE)
adddoc_table$file <- basename(adddoc_table$Doc_Link)
adddoc_table$file <- ifelse(is.na(adddoc_table$file) |
                               grepl(adddoc_table$file, pattern = "pdf$|PDF$"),
                            adddoc_table$file,
                            paste0(adddoc_table$file, ".pdf"))
adddoc_table$file <- clean_fname(adddoc_table$file)
OCR_res$file <- basename(OCR_res$infile)
fr_j1 <- left_join(adddoc_table, OCR_res, by = "file")
fr_j2 <- distinct(fr_j1, Doc_Link, Species, OCR_MD5, .keep_all = TRUE)
fr_j3 <- select(fr_j2, -error, -proc_time)
names(fr_j3)

fr_spp <- aggregate(Species ~ Doc_Link, data = fr_j3, FUN = unique)
fr_dat1 <- left_join(fr_spp, fr_j3, by = "Doc_Link")
spp_links <- distinct(fr_dat1, Species.x, .keep_all = TRUE)
spp_links <- filter(spp_links, Doc_Link != "http://ecos.fws.gov")
spp_links$local_pdf <- gsub(spp_links$outfile,
                            pattern = "/datadrive/data",
                            replacement = "https://defend-esc-dev.org")
spp_links$local_txt <- gsub(spp_links$local_pdf,
                            pattern = "ESAdocs",
                            replacement = "ESAdocs_text")
spp_links$local_txt <- gsub(spp_links$local_txt,
                            pattern = "pdf$|PDF$",
                            replacement = "txt")
adddoc_elast <- select(spp_links, -Species.y, -infile, -outfile)
adddoc_elast$Date <- as.Date(adddoc_elast$Date)
names(adddoc_elast)
names(adddoc_elast) <- c("link", "species", "date", "fr_citation_page",
                         "title", "tags", "file_name", "pdf_md5", "n_pages",
                         "pdf_path", "txt_path")
adddoc_elast$type <- "federal_register"
adddoc_elast$raw_txt <- adddoc_elast$geo <- ""
save(adddoc_elast, file = "~/Work/Data/esadocs/rda/adddoc_elast.rda")

# Five-year review docs
fiveyr_table$file <- basename(fiveyr_table$Doc_Link)
fiveyr_table$file <- ifelse(is.na(fiveyr_table$file) |
                               grepl(fiveyr_table$file, pattern = "pdf$|PDF$"),
                            fiveyr_table$file,
                            paste0(fiveyr_table$file, ".pdf"))
fiveyr_table$file <- clean_fname(fiveyr_table$file)
OCR_res$file <- basename(OCR_res$outfile)
fr_j1 <- left_join(fiveyr_table, OCR_res, by = "file")
fr_j2 <- distinct(fr_j1, Doc_Link, Species, OCR_MD5, .keep_all = TRUE)
fr_j3 <- select(fr_j2, -error, -proc_time)
names(fr_j3)

fr_spp <- aggregate(Species ~ Doc_Link, data = fr_j3, FUN = unique)
fr_dat1 <- left_join(fr_spp, fr_j3, by = "Doc_Link")
spp_links <- distinct(fr_dat1, Species.x, .keep_all = TRUE)
spp_links <- filter(spp_links, Doc_Link != "http://ecos.fws.gov")
spp_links$local_pdf <- gsub(spp_links$outfile,
                            pattern = "/datadrive/data",
                            replacement = "https://defend-esc-dev.org")
spp_links$local_txt <- gsub(spp_links$local_pdf,
                            pattern = "ESAdocs",
                            replacement = "ESAdocs_text")
spp_links$local_txt <- gsub(spp_links$local_txt,
                            pattern = "pdf$|PDF$",
                            replacement = "txt")
fiveyr_elast <- select(spp_links, -Species.y, -infile, -outfile)
fiveyr_elast$Date <- as.Date(fiveyr_elast$Date)
names(fiveyr_elast)
names(fiveyr_elast) <- c("link", "species", "date", "title", "file_name",
                         "pdf_md5", "n_pages", "pdf_path", "txt_path")
fiveyr_elast$type <- "five_year_review"
fiveyr_elast$raw_txt <- fiveyr_elast$geo <- fiveyr_elast$tags <- ""
save(fiveyr_elast, file = "~/Work/Data/esadocs/rda/fiveyr_elast.rda")

# Consultations...a bit different
consult_files <- filter(OCR_res, grepl(OCR_res$outfile, pattern = "/consultation/"))
names(consult_files)
consult_files <- select(consult_files, -infile, -error, -proc_time)
names(consult_files)
names(consult_files) <- c("pdf_path", "pdf_md5", "n_pages", "file_name")
consult_files$pdf_path <- gsub(consult_files$pdf_path,
                               pattern = "/datadrive/data",
                               replacement = "https://defend-esc-dev.org")
consult_files$txt_path <- gsub(consult_files$pdf_path,
                               pattern = "ESAdocs",
                               replacement = "ESAdocs_text")
consult_files$type <- "consultation"
consult_files$link <- consult_files$date <- consult_files$title <- NA
consult_files$raw_txt <- ""
consult_files$activity_code <- consult_files$geo <- consult_files$tags <- NA
consult_files$species <- consult_files$federal_agency <- ""
names(consult_files)
consult_elast <- consult_files[, c(9,6,8,7,4,1,5,10,2:3,14,13,15,12,11)]
save(consult_elast, file = "~/Work/Data/esadocs/rda/consult_elast.rda")

# Conservation agreements
consag_files <- filter(OCR_res, grepl(OCR_res$outfile, pattern = "/conserv_agmt/"))
names(consult_files)
consag_files <- select(consag_files, -infile, -error, -proc_time)
names(consag_files)
names(consag_files) <- c("pdf_path", "pdf_md5", "n_pages", "file_name")
consag_files$pdf_path <- gsub(consag_files$pdf_path,
                              pattern = "/datadrive/data",
                              replacement = "https://defend-esc-dev.org")
consag_files$txt_path <- gsub(consag_files$pdf_path,
                              pattern = "ESAdocs",
                              replacement = "ESAdocs_text")
consag_files$type <- "conserv_agmt"
consag_files$link <- consag_files$date <- consag_files$title <- NA
consag_files$raw_txt <- consag_files$species <- ""
consag_files$geo <- consag_files$tags <- NA
names(consag_files)
consag_elast <- consag_files
names(consag_elast)
save(consag_elast, file = "~/Work/Data/esadocs/rda/consag_elast.rda")

