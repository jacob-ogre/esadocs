# BSD_2_clause

shinyServer(function(input, output) {

  rv <- reactiveValues(current_page = 1)

  observe({
    toggleState(id = "prevButton", condition = rv$current_page > 1)
    toggleState(id = "nextButton", condition = rv$current_page < n_pages())
    hide(selector = ".page")
    # print(generate_hits(cur_res())[[rv$current_page]])
    # print(rv$current_page)
    show("hits")
    # show(sprintf("pg%s", rv$current_page))
    # output$hits <- renderUI({
    #   show(generate_hits(cur_res())[[rv$current_page]][[1]])
    # })
  })

  navPage <- function(direction) {
    rv$current_page <- rv$current_page + direction
  }

  observeEvent(input$prevButton, navPage(-1))
  observeEvent(input$nextButton, navPage(1))

  output$n_docs <- renderText({
    tmp <- index_stats("esadocs2")$indices$esadocs2$total$docs$count
    return(paste(tmp, "documents indexed"))
  })

  srch_len <- reactive({
    if(input$show_n == "Hits per page") {
      return(10)
    } else {
      return(as.numeric(input$show_n))
    }
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
      params = list(my_field = "raw_txt",
                    my_value = cur_input(),
                    my_size = 20L)
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

  hit_page <- function(i, data, pg) {
    div(id = paste0("pg", pg),
      div(class = "search-res",
        fluidRow(
          column(10,
            span(
              ifelse(nchar(data$Title[i]) > 10,
                           data$Title[i],
                           "No document title"),
              style = "font-size:larger;font-weight:bold"
            ),
            div(
              a(href = data$link[i],
                ifelse(nchar(data$link[i]) < 40,
                       data$link[i],
                       str_trunc(data$link[i], 40))),
              p(HTML(data$highlight[i])),
              helpText(paste("Score:", data$Score[i])))
          ),
          column(2,
            div(a(href = data$link[i],
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
  }

  n_pages <- reactive({
    n_hits <- length(cur_res()[,1])
    n_pages <- n_hits %/% srch_len()
    if(n_hits %% srch_len() != 0) {
      n_pages <- n_pages + 1
    }
    return(n_pages)
  })

  generate_hits <- function(dat) {
    n_hits <- length(dat[,1])
    n_pages <- n_pages()
    page_ls <- as.list(rep(NA, n_pages))
    pages <- 1:n_pages
    breaks <- seq(1, n_hits, srch_len())
    if(length(breaks) == 1) {
      page_ls[[1]] <- lapply(1:length(dat[, 1]), hit_page, data = dat, pg = 1)
    } else {
      for(j in pages[1:n_pages() - 1]) {
          cur_st <- breaks[pages[j]]
          cur_en <- breaks[pages[j + 1]] - 1
          cur_set <- dat[cur_st:cur_en, ]
          page_ls[[j]] <- lapply(1:length(cur_set[, 1]),
                                 hit_page,
                                 data = dat,
                                 pg = j)
      }
      cur_st <- breaks[length(breaks)]
      cur_en <- length(dat[,1])
      observe({ print(c(cur_st, cur_en)) })
      page_ls[[length(pages)]] <- lapply(1:length(dat[cur_st:cur_en, 1]),
                                         hit_page,
                                         data = dat[cur_st:cur_en, ],
                                         pg = length(pages))
    }
    return(page_ls)
  }

  output$hits <- renderUI({
    if(test_nulls(cur_res())) {
      h4("No matches; please enter another search.")
    } else {
      pages <- generate_hits(cur_res())
      # observe({ print(pages[[rv$current_page]]) })
      return(pages[[rv$current_page]])
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
