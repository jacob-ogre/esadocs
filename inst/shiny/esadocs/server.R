# BSD_2_clause

shinyServer(function(input, output) {
  output$n_docs <- renderText({
    tmp <- Search(index = "esadocs",
                  fields = c("txt", "pdf"),
                  asdf = TRUE)$hits$hits
    num <- dim(tmp)[1]
    return(paste(num, "documents indexed"))
  })

  replace_chars <- function(x) {
    x <- stringr::str_replace_all(x,
                                  pattern = "\\{|\\}|;",
                                  replacement = " ")
    observe({ print(x) })
    return(x)
  }

  cur_input <- reactive({
    return(replace_chars(input$main_input))
  })

  cur_res <- eventReactive(input$search, {
    cur_res <- Search(index = "esadocs",
           type = "federal_register",
           q = cur_input(),
           fields = c("txt", "pdf", "basename"),
           asdf = TRUE)$hits$hits
  })

  check_search <- reactive({
    a_res <- try(length(cur_res()[[1]]), silent = TRUE)
    if(class(a_res) != "try-error") TRUE else FALSE
  })

  n_match <- reactive({
    if(check_search()) {
      return(length(cur_res()[[1]]))
    } else {
      return(NA)
    }
  })

  output$n_hits <- renderText({
    if(!is.na(n_match())) {
      if(n_match() > 1){
        return(paste(n_match(), "matches"))
      } else {
        return(paste(n_match(), "match"))
      }
    } else {
      return("")
    }
  })

  output$hits <- renderUI({
    if(!is.na(n_match())) {
      lapply(1:n_match(), function(i) {
        box(
          title = div(cur_res()$fields$basename[i],
                      style = "color:blue;font-size:larger;font-weight:bold"),
          status = "primary",
          width = 12,
          div(span("Text doc:", style = "font-weight:bold"),
              cur_res()$fields$txt[i]),
          div(span("PDF doc:", style = "font-weight:bold"),
              cur_res()$fields$pdf[i]),
          helpText(paste("Score:", cur_res()$`_score`[i]))
        )
      })
    } else if(class(cur_res()) == "list") {
      box(
        title = div("No matches",
                    style = "color:orange;font-size:larger"),
        status = "warning",
        width = 12,
        p("Please try another search.")
      )
    } else {
      h3("No results.")
    }
  })

})
