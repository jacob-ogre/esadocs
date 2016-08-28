library(elastic)

connect()
shakespeare <- system.file("examples",
                           "shakespeare_data.json",
                           package = "elastic")
docs_bulk(shakespeare)

plosdat <- system.file("examples", "plos_data.json", package = "elastic")
docs_bulk(plosdat)

t1 <- Search(index = "plos", size = 4)

get_titles <- function(x) {
  tmp <- lapply(x[["hits"]][["hits"]], FUN = `[[`, 5)
  return(unlist(lapply(tmp, FUN = `[[`, "title")))
}
get_titles(t1)

t2 <- Search(index = "plos", size = 10)
get_titles(t2)

docs_get(index='plos', type='article', id=1, fields='title')$fields$title[[1]]

plosdat

test <- Search(index = "plos",
               # fields = 'title',
               type = 'article',
               q = 'population genetic',
               size = 100,
               asdf = TRUE)$hits$hits
test$id <- test$`_source`$id
test$title <- test$`_source`$title
test <- dplyr::select(test, `_index`, `_type`, `_id`, `_score`, id, title)
head(test)

test2 <- Search(index = "plos",
               # fields = 'title',
               type = 'article',
               q = 'Population Genetic',
               size = 100,
               asdf = TRUE)$hits$hits
test2$id <- test2$`_source`$id
test2$title <- test2$`_source`$title
test2 <- dplyr::select(test2, `_index`, `_type`, `_id`, `_score`, id, title)
head(test2)

test3 <- Search(index = "plos",
               # fields = 'title',
               type = 'article',
               q = '*acea',
               size = 100,
               asdf = TRUE)$hits$hits
test3$id <- test3$`_source`$id
test3$title <- test3$`_source`$title
test3 <- dplyr::select(test3, `_index`, `_type`, `_id`, `_score`, id, title)
head(test3)

test4 <- Search(index = "plos",
               # fields = 'title',
               type = 'article',
               q = 'Population Genetic',
               size = 100,
               raw = TRUE)
test4_df <- jsonlite::fromJSON(test4)$hits$hits

#############################################################################
# trying out some of my own data
docs_bulk(qq, type = "federal_register")

test5 <- Search(index = "esadocs",
                type = "federal_register",
                q = "chiricahua",
                asdf = TRUE)$hits$hits
dim(test5)
system(paste("open", test5$`_source`$pdf))

test6 <- Search(index = "esadocs",
                type = "federal_register",
                q = "fish and wildlife service",
                asdf = TRUE)$hits$hits
dim(test6)
test6$`_source`$txt

test7 <- Search(index = "esadocs",
                type = "federal_register",
                q = "oaks",
                fields = "pdf",
                asdf = TRUE)$hits$hits
dim(test7)

test8 <- Search(index = "esadocs",
                type = "federal_register",
                q = "recovery AND unit",
                fields = "pdf",
                asdf = TRUE)$hits$hits
dim(test8)
system(paste("open", test8$fields$pdf[1][[1]][1]))
system(paste("open", test8$fields$pdf[2][[1]][1]))
