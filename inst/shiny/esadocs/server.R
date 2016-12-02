# BSD_2_clause

shinyServer(function(input, output, session) {

  rv <- reactiveValues(current_page = 1)

  observeEvent(input$search, {
    withBusyIndicatorServer("search", {
      Sys.sleep(2.5)
    })
  })

  # observer for incrementing pages
  observe({
    toggleState(id = "prevButton", condition = rv$current_page > 1)
    toggleState(id = "nextButton", condition = rv$current_page < n_pages())
    hide(selector = ".page")
    shinyjs::show("hits",
                  anim = TRUE,
                  animType = "fade",
                  time = 0.25)
  })

  navPage <- function(direction) {
    rv$current_page <- rv$current_page + direction
  }

  observeEvent(input$prevButton, navPage(-1))
  observeEvent(input$nextButton, navPage(1))
  observeEvent(input$tog_extras,
               toggle("extras",
                      anim = TRUE,
                      animType = "fade",
                      time = 0.25))

  # the number of indexed documents; could change to an option() later
  output$n_docs <- renderText({
    tmp <- try(index_stats("esadocs")$indices$esadocs$primaries$docs$count,
               silent = TRUE)
    if(class(tmp) == "try-error") {
      cmd <- "/home/jacob/elasticsearch-2.4.0/bin/elasticsearch -d"
      res <- try(system(cmd, intern = TRUE), silent = TRUE)
      withProgress(
        message = "Restarting elasticsearch",
        detail = "This may take a moment...",
        value = 0, {
          for(i in 1:6) {
            incProgress(1/6)
            Sys.sleep(1)
          }
        }
      )
      shinyjs::show("reset_srv")
      return("The elasticsearch server may be down; please try to restart.")
    }
    return(paste(tmp, "documents indexed"))
  })

  observeEvent(input$reset_btn, {
    cmd <- "/home/jacob/elasticsearch-2.4.0/bin/elasticsearch -d"
    res <- try(system(cmd, intern = TRUE), silent = TRUE)
    observe({ print(class(res)) })
    withProgress(
      message = "Restarting elasticsearch",
      detail = "This may take a moment...",
      value = 0, {
        for(i in 1:4) {
          incProgress(1/4)
          Sys.sleep(1)
        }
      }
    )
    tmp <- try(index_stats("esadocs")$indices$esadocs$primaries$docs$count,
               silent = TRUE)
    if(class(tmp) == "try-error") {
      withProgress(
        message = "Elasticsearch starting",
        detail = "Still trying...",
        value = 0, {
          for(i in 1:4) {
            incProgress(1/4)
            Sys.sleep(1)
          }
        }
      )
      tmp <- try(index_stats("esadocs")$indices$esadocs$primaries$docs$count,
                 silent = TRUE)
      if(class(tmp) == "try-error") {
        shinyjs::hide("reset_srv")
        output$n_docs <- renderText({
          HTML("<span style='color:red; font-weight:bold'>
                Please <a href='mailto:esa@defenders.org'>contact us</a>,
                something is amiss...</span>")
        })
      } else {
        shinyjs::hide("reset_srv")
        output$n_docs <- renderText(paste(tmp, "documents indexed"))
      }
    } else {
      shinyjs::hide("reset_srv")
      output$n_docs <- renderText(paste(tmp, "documents indexed"))
    }
  })

  # set the number of results per page
  srch_len <- reactive({
    if(input$show_n == "Hits per page (10)") {
      return(10)
    } else {
      return(as.numeric(input$show_n))
    }
  })

  # placeholder function for cleaning input; will probably be extended
  replace_chars <- function(x) {
    x <- gsub(x, pattern = '"', replacement = '\\"', fixed = TRUE)
    return(x)
  }

  # reactive form of input$main_input
  cur_input <- reactive({
    return(replace_chars(input$main_input))
  })

  cur_type <- reactive({
    if(input$type_filt == "all") {
      return("")
    } else {
      return(input$type_filt)
    }
  })

  # convert input$date_filt into a Date
  date_from <- reactive({
    as.Date(as.numeric(input$date_filt[1]), origin = "1970-01-01")
  })

  date_to <- reactive({
    as.Date(as.numeric(input$date_filt[2]), origin = "1970-01-01")
  })

  # MAIN SEARCH FUNCTION; note 500-result limit at this time, very simple search
  # function that needs to be beefed up
  cur_res <- eventReactive(input$search, {
    if(input$main_input == "") return(NULL)
    body <- list(
              min_score = 0.1,
              query = list(
                match = list(
                  raw_txt.shingles = cur_input()
                )
              ),
              size = 500,
              highlight = list(
                fields = list(
                  raw_txt.shingles = list(
                    `type` = "fvh",
                    `fragment_size` = 150,
                    `pre_tags` = list("<b>"),
                    `post_tags` = list("</b>")
                  )
                )
              )
    )
    # fgh <- index_clear_cache("esadocs")
    cur_mats <- Search("esadocs",
                       type = cur_type(),
                       body = body)$hits$hits
    if(length(cur_mats) > 0) {
      res_df <- result_asdf(cur_mats)
      res_df$highlight <- get_highlight(cur_mats)
      res_df <- distinct(res_df, pdf, .keep_all = TRUE)
      res_df <- filter(res_df,
                       is.na(res_df$date) |
                       (res_df$date >= date_from() &
                        res_df$date <= date_to()))
        if(input$type_filt != "all") {
        res_df <- filter(res_df, res_df$type == input$type_filt)
      }
      if(length(res_df[,1]) > 0) {
        return(res_df)
      } else {
        return(NA)
      }
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

  output$n_hits <- renderText({
    if(test_nulls(cur_res())) {
      return("")
    } else if(n_match() > 1) {
      return(paste(n_match(), "matches"))
    } else {
      return(paste(n_match(), "match"))
    }
  })

  # MAIN HTML GENERATION
  hit_page <- function(i, data, pg) {
    div(id = paste0("pg", pg),
      div(class = "search-res",
        fluidRow(
          column(10,
            a(href = data$link[i],
              target = "_blank",
              span(
                ifelse(nchar(data$title[i]) > 10 & !is.na(data$title[i]),
                       data$title[i],
                       "No document title"),
                style = "font-size:larger;font-weight:bold"
              )
            ),
            fluidRow(
              column(3,
                div(class = "info-div",
                    icon("file-text-o"),
                    str_replace_all(data$type[i], "_", " "))
              ),
              column(3,
                div(class = "info-div",
                    icon("calendar"),
                    data$date[i])
              ),
              column(3,
                div(class = "info-div",
                    icon("star"),
                    paste("Score:", round(data$score[i], 3)))
              ),
              column(3)
            ),
            fluidRow(
              column(12,
                HTML(data$highlight[i])
              )
            ),
            fluidRow(
              column(2,
                popify(
                  actionLink(
                    inputId = paste0("spp", i),
                    label = div(style = "font-weight:300;",
                                "Species"),
                    style = "default"
                  ),
                  title = "Relevant species",
                  content = HTML(data$species[i]),
                  placement = "right",
                  trigger = "focus"
                )
              ),
              column(2,
                popify(
                  actionLink(
                    inputId = paste0("state", i),
                    label = div(style = "font-weight:300;",
                                "States"),
                    style = "default"
                  ),
                  title = "Relevant U.S. states",
                  content = paste(
                    "Not yet calculated. Future iterations will calculate the",
                    "states relevant to the document based on the occurrence",
                    "of species referenced in the document."),
                  placement = "right",
                  trigger = "focus"
                )
              )
            )
          ),
          column(2)
        )
        ),
        div(style = "background:rgba(0,0,0,0)",
          br()
        )
      )
  }

  n_pages <- reactive({
    if(!test_nulls(cur_res())) {
      n_hits <- length(cur_res()[,1])
      n_pages <- n_hits %/% srch_len()
      if(n_hits %% srch_len() != 0) {
        n_pages <- n_pages + 1
      }
      return(n_pages)
    } else {
      return(1)
    }
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
                               data = dat[cur_st:cur_en, ],
                               pg = j)
      }
      cur_st <- breaks[length(breaks)]
      cur_en <- length(dat[,1])
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
      if(length(pages) > 1) {
        shinyjs::show("prevButton")
        shinyjs::show("res_txt")
        shinyjs::show("nextButton")
      } else {
        shinyjs::hide("prevButton")
        shinyjs::hide("res_txt")
        shinyjs::hide("nextButton")
      }
      if(length(pages) < rv$current_page) rv$current_page <- 1
      return(pages[[rv$current_page]])
    }
  })

  output$summary_figs <- renderUI({
    if(test_nulls(cur_res())) {
      shinyjs::hide("prevButton")
      shinyjs::hide("res_txt")
      shinyjs::hide("nextButton")
      shinyjs::hide("summ_head")
      return(NULL)
    }
    shinyjs::show("summ_head")

    output$score_plot <- renderPlot({
      dat <- data.frame(score = cur_res()$score)
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      p <- ggplot(data = dat, aes(score)) +
             geom_histogram(bins = nbin) +
             labs(x = "Search score",
                  y = "# documents") +
             theme_hc()
      return(p)
    })

    output$date_plot <- renderPlot({
      dat <- data.frame(date = as.Date(cur_res()$date))
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      p <- ggplot(data = dat, aes(date)) +
             geom_histogram(bins = nbin) +
             labs(x = "Document date",
                  y = "# documents") +
             theme_hc()
      return(p)
    })

    output$top_spp <- renderPlot({
    spp_list <- str_split(
      paste(cur_res()$species, collapse = "<br>"),
      "<br>")
    spp_tab <- sort(table(spp_list), decreasing = TRUE)[1:10]
    spp_df <- data.frame(species = names(spp_tab),
                         count = as.vector(spp_tab),
                         stringsAsFactors = FALSE)
    p <- ggplot(spp_df, aes(x = species, y = count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "",
           y = "# documents",
           subtitle = "Top 10 species") +
      theme_hc()
    return(p)
    })

    tabBox(
      tabPanel(
        title = "Score",
        plotOutput("score_plot", height = "250px")
      ),
      tabPanel(
        title = "Date",
        plotOutput("date_plot", height = "250px")
      ),
      tabPanel(
        title = "Species",
        plotOutput("top_spp", height = "250px")
      ),
      width = 12,
      height = "200px"
    )
  })

})
