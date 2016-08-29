# BSD_2_clause

library(dplyr)
library(elastic)
library(shiny)
library(shinydashboard)
library(shinyBS)

elastic::connect()

searched <- FALSE
