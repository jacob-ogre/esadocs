# BSD_2_clause

library(dplyr)
library(stringr)


fils <- list.files("~/Work/Data/esadocs/rda",
                   recursive = TRUE,
                   full.names = TRUE)

# ECOS_species_links
load(fils[4])

# many tables: adddoc_table, CCA_table, CCAA_table, crithab_table, fedreg_table,
#   fiveyr_table, HCP_table, HCP_table, recovery_table, SHA_table, species_table
load(fils[5])

# for_OCR (12 Dec)
load(fils[7])



# let's first get the PDF links out
pdfs <- filter(ECOS_species_links,
               grepl(ECOS_species_links$link, pattern = "pdf$|PDF$|gpo"))
pdfs$file <- basename(pdfs$link)

fedreg_v2 <- left_join()
