# BSD_2_clause
#
# Somewhere in the melee of loading all of the docs and filling in the missing
# docs duplicates arose. This is a script to clean those up.

library(dplyr)
library(elastic)
library(esadocs)
library(purrr)

connect()

# consultations
consult <- Search("esadocs", type = "consultation", size = 5000)
cons_res <- result_asdf(consult$hits$hits)
cons_res$id <- sapply(1:dim(cons_res)[1],
                      function(x) consult$hits$hits[[x]]$`_id`)
na_md5 <- filter(cons_res, is.na(cons_res$pdf_md5)) %>%
  distinct(file_name, .keep_all = TRUE)
no_dup <- distinct(cons_res, pdf_md5, .keep_all = TRUE) %>%
  filter(!is.na(pdf_md5))
cons_keep <- rbind(no_dup, na_md5)
to_remove <- setdiff(cons_res$id, cons_keep$id)

bulk_delete <- function(idx, ids) {
  tfn <- function(idx, id) {
    pres <- try(docs_get("esadocs", idx, id = id)$found, silent = TRUE)
    if(class(pres) != "try-error") {
      res <- try(docs_delete("esadocs", idx, id = id), silent = TRUE)
      if(class(res) == "try-error") {
        return("delete_error")
      }
      return("delete_success")
    }
    return("no_record")
  }

  res <- sapply(ids, FUN = tfn, idx = idx)
  return(res)
}
consult_del <- bulk_delete(idx = "consultation", ids = to_remove)

fedreg <- Search("esadocs", type = "federal_register", size = 5000)
fr_res <- result_asdf(fedreg$hits$hits)
fr_res$id <- sapply(1:dim(fr_res)[1],
                      function(x) fedreg$hits$hits[[x]]$`_id`)
na_md5 <- filter(fr_res, is.na(fr_res$pdf_md5)) %>%
  distinct(file_name, .keep_all = TRUE)
no_dup <- distinct(fr_res, pdf_md5, .keep_all = TRUE) %>%
  filter(!is.na(pdf_md5))
cons_keep <- rbind(no_dup, na_md5)
to_remove <- setdiff(fr_res$id, cons_keep$id)
fedreg_del <- bulk_delete(idx = "federal_register", ids = to_remove)

recpln <- Search("esadocs", type = "recovery_plan", size = 5000)
rp_res <- result_asdf(recpln$hits$hits)
rp_res$id <- sapply(1:dim(rp_res)[1],
                      function(x) recpln$hits$hits[[x]]$`_id`)
na_md5 <- filter(rp_res, is.na(rp_res$pdf_md5)) %>%
  distinct(file_name, .keep_all = TRUE)
no_dup <- distinct(rp_res, pdf_md5, .keep_all = TRUE) %>%
  filter(!is.na(pdf_md5))
cons_keep <- rbind(no_dup, na_md5)
to_remove <- setdiff(rp_res$id, cons_keep$id)
recpln_del <- bulk_delete(idx = "recovery_plan", ids = to_remove)

