# BSD_2_clause

library(digest)
library(dplyr)
library(ecosscraper)
library(tools)

data("ecos_doc_links")
data("TECP_domestic")
data("federal_register_table")

head(data.frame(ecos_doc_links))
ecos_doc_links <- filter(ecos_doc_links,
                         Scientific_Name %in% unique(TECP_domestic$Scientific_Name))
fedreg <- filter(ecos_doc_links, type == "federal_register")
recpln <- filter(ecos_doc_links, type == "recovery_plan")
fiveyr <- filter(ecos_doc_links, type == "five_year_review")
test <- rbind(head(fedreg), head(recpln), head(fiveyr))
names(test) <- c("Species", "Doc_Link", "link", "text", "type")
names(federal_register_table)

# First need to test that the loading will work as I think it will work
tmp <- head(fedreg, 25)
names(tmp) <- c("Species", "Doc_Link", "link", "text", "type")

##########
qq <- left_join(tmp, federal_register_table, by = c("Doc_Link", "Species"))
# Well crap...this test join highlights a problem from earlier (in ecosscraper)
# that I don't think I can get around. I haven't figured out a clean way to get
# all of the information in tables out without separate calls for the text (<a>)
# and for the links (href), then joining those tables on the text. When there
# are two different docs with the same Title then that doc/link gets double-
# counted. In this case, a proposed rule and a final rule have the same title,
# inflating the document count from 25 to 27. There is no way to pick the correct
# a:href pair to retain.
######

qq <- select(qq, -text)
qq$pdf <- unlist(lapply(lapply(qq$link, make_file_paths), `[[`, 1))
qq$txt <- unlist(lapply(lapply(qq$link, make_file_paths), `[[`, 2))
qq$pdf_path <- paste0("~/esadocs/", qq$type, "/PDFs/", qq$pdf)
qq$txt_path <- paste0("~/esadocs/", qq$type, "/TXTs/", qq$txt)
qq$raw_txt <- unlist(lapply(qq$txt_path, load_doc_text))
qq$pdf_md5 <- md5sum(normalizePath(qq$pdf_path))
qq$pdf_size <- file.size(normalizePath(qq$pdf_path))

# One missing field is the species that are covered by a given document
spp <- aggregate(Scientific_Name ~ href, data = fedreg, FUN = unique)
tt <- left_join(qq, spp, by = c("Doc_Link" = "href"))
rr <- select(tt, -Species)
ee <- distinct(rr, pdf, .keep_all = TRUE)
ee <- select(ee, -Doc_Link)



