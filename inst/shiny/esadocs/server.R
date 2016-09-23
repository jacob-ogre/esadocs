# BSD_2_clause

shinyServer(function(input, output) {

  output$n_docs <- renderText({
    tmp <- index_stats("esadocs2")$indices$esadocs2$total$docs$count
    return(paste(tmp, "documents indexed"))
  })

  replace_chars <- function(x) {
    x <- stringr::str_replace_all(x,
                                  pattern = "\\{|\\}|;",
                                  replacement = " ")
    return(x)
  }

  cur_input <- reactive({
    return(replace_chars(input$main_input))
  })

  cur_res <- eventReactive(input$search, {
    if(input$main_input == "") return(NULL)
    srch <- cur_input()
    body <- list(
      inline = list(query = list(match = list(`{{my_field}}` = "{{my_value}}")),
                    size = "{{my_size}}",
                    highlight = list(
                      fields = list(
                        `{{my_field}}` = list(
                          `fragment_size` = 150,
                          `pre_tags` = list("<b>"),
                          `post_tags` = list("</b>")
                          )
                        )
                      )
                    ),
      params = list(my_field = "raw_txt", my_value = cur_input(), my_size = 20L)
    )
    cur_mats <- Search_template(body = body)$hits$hits
    if(length(cur_mats) > 0) {
      res_df <- result_asdf(cur_mats)
      res_df$highlight <- get_highlight(cur_mats)
      return(res_df)
    } else {
      return(NA)
    }
  })

  n_match <- reactive({
    a_res <- try(length(cur_res()[[1]]), silent = TRUE)
    if(class(a_res) != "try-error") {
      return(length(cur_res()[[1]]))
    } else {
      return(0)
    }
  })

  test_nulls <- function(x) {
    if(class(x) == "NULL" |
       class(x) == "NA" |
       class(x) == "logical") {
      return(TRUE)
    }
    return(FALSE)
  }

  output$n_hits <- renderText({
    if(test_nulls(cur_res())) {
      return("")
    } else if(n_match() > 1) {
      return(paste(n_match(), "matches"))
    } else {
      return(paste(n_match(), "match"))
    }
  })

  output$hits <- renderUI({
    if(test_nulls(cur_res())) {
      h4("No matches; please enter a search.")
    } else if(class(cur_res()) != "NA") {
      lapply(1:n_match(), function(i) {
        div(div(class = "search-res",
          fluidRow(
            column(10,
              span(
                ifelse(nchar(cur_res()$Title[i]) > 10,
                             cur_res()$Title[i],
                             "No document title"),
                style = "font-size:larger;font-weight:bold"
              ),
              div(
                a(href = cur_res()$link[i],
                  ifelse(nchar(cur_res()$link[i]) < 40,
                         cur_res()$link[i],
                         str_trunc(cur_res()$link[i], 40))),
                p(HTML(cur_res()$highlight[i])),
                helpText(paste("Score:", cur_res()$Score[i])))
            ),
            column(2,
              div(a(href = cur_res()$link[i],
                    target = "_blank",
                    style = "color:#cc0000",
                    icon("file-pdf-o", "fa-2x"))
              )
            )
          )),
          div(style = "background:rgba(0,0,0,0)",
            br()
          )
        )
      })
    }
  })

  output$score_dist <- renderPlot({
    if(test_nulls(cur_res())) return(NULL)
    if(input$summary_plot == "Score") {
      shinyjs::show("selector")
      dat <- data.frame(score = cur_res()$Score)
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      p <- ggplot(data = dat, aes(score)) +
             geom_histogram(bins = nbin) +
             labs(x = "Score",
                  y = "# documents",
                  title = "Score distribution",
                  subtitle = "higher = better") +
             theme_hc()
      return(p)
    } else if(input$summary_plot == "Date") {
      shinyjs::show("selector")
      dat <- data.frame(date = as.Date(cur_res()$Date))
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      p <- ggplot(data = dat, aes(date)) +
             geom_histogram(bins = nbin) +
             labs(x = "Date",
                  y = "# documents",
                  title = "Date distribution",
                  subtitle = "higher = better") +
             theme_hc()
      return(p)
    }
  })

})
