# BSD_2_clause

shinyServer(function(input, output) {

  replace_chars <- function(x) {
    x <- stringr::str_replace_all(x,
                                  pattern = "\\{|\\}|;",
                                  replacement = " ")
    return(x)
  }

  cur_input <- reactive({
    return(replace_chars(input$main_input))
  })

  in_type
    if(grepl(inpu, pattern = "type:") {
      in_type <-
    }
  })
  test8 <- Search(index = "esadocs",
                  type = "federal_register",
                  q = "recovery AND unit",
                  fields = "pdf",
                  asdf = TRUE)$hits$hits

})
