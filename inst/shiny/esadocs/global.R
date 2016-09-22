# BSD_2_clause

library(dplyr)
library(elastic)
library(esadocs)
library(ggplot2)
library(ggthemes)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(stringr)

elastic::connect()

searched <- FALSE

varnames <- c("link", "type", "Date", "Citation.Page", "Title",
              "pdf", "txt", "pdf_path", "txt_path", "raw_txt",
              "pdf_md5", "pdf_size", "species", "Score" )
