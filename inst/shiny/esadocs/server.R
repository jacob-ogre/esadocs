# BSD_2_clause

shinyServer(function(input, output, session) {

  rv <- reactiveValues(current_page = 1)

  # observer for incrementing pages
  observe({
    toggleState(id = "prevButton", condition = rv$current_page > 1)
    toggleState(id = "nextButton", condition = rv$current_page < n_pages())
    hide(selector = ".page")
    show("hits",
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
    tmp <- index_stats("esadocs2")$indices$esadocs2$total$docs$count
    return(paste(tmp, "documents indexed"))
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
    x <- str_replace_all(x,
                         pattern = "\\{|\\}|;",
                         replacement = " ")
    return(x)
  }

  # reactive form of input$main_input
  cur_input <- reactive({
    return(replace_chars(input$main_input))
  })

  # convert input$date_filt into a Date
  date_filter <- reactive({
    lapply(input$date_filt, as.Date)
  })

  # MAIN SEARCH FUNCTION; note 50-result limit at this time, very simple search
  # function that needs to be beefed up
  cur_res <- eventReactive(input$search, {
    if(input$main_input == "") return(NULL)
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
                    my_size = 50L)
    )
    cur_mats <- Search_template(body = body)$hits$hits
    if(length(cur_mats) > 0) {
      res_df <- result_asdf(cur_mats)
      res_df$highlight <- get_highlight(cur_mats)
      ######### REMOVE AFTER CORRECT LOADING!
      res_df$Date <- as.Date(res_df$Date)
      #########
      res_df <- filter(res_df,
                       is.na(res_df$Date) |
                       (res_df$Date >= date_filter()[1] &
                        res_df$Date <= date_filter()[2]))
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

  # Test for NULL/NA/logical classes; used where these classes aren't expected
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

  # MAIN HTML GENERATION
  hit_page <- function(i, data, pg) {
    div(id = paste0("pg", pg),
      div(class = "search-res",
        fluidRow(
          column(10,
            a(href = data$link[i],
              target = "_blank",
              span(
                ifelse(nchar(data$Title[i]) > 10 & !is.na(data$Title[i]),
                       data$Title[i],
                       "No document title"),
                style = "font-size:larger;font-weight:bold"
              )
            ),
            fluidRow(
              column(3,
                div(class = "info-div",
                    icon("file-text-o"),
                    str_replace(data$type[i], "_", " "))
              ),
              column(3,
                div(class = "info-div",
                    icon("calendar"),
                    data$Date[i])
              ),
              column(3,
                div(class = "info-div",
                    icon("star"),
                    paste("Score:", round(data$Score[i], 3)))
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
                  content = HTML(data$Species[i]),
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
          column(2 #,
            # div(
            #   a(href = data$link[i],
            #     target = "_blank",
            #     style = "color:#cc0000",
            #     icon("file-pdf-o", "fa-2x"),
            #     "PDF"
            #   )
            # ),
            # br(),
            # popify(
            #   div(
            #     actionLink(
            #       inputId = paste0("rawtxt", i),
            #       label = NULL,
            #       icon = icon("file-text-o", "fa-2x"),
            #       "TXT"
            #     )
            #   ),
            #   title = "Raw text",
            #   content = paste(
            #     str_sub(data$raw_txt[i], 1, 750),
            #     "....."),
            #   placement = "right",
            #   trigger = "focus"
            # )
          )
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
        show("prevButton")
        show("res_txt")
        show("nextButton")
      }
      # observe({ print(pages) })
      return(pages[[rv$current_page]])
    }
  })

  output$summary_figs <- renderUI({
    if(test_nulls(cur_res())) return(NULL)
    show("summ_head")
    output$score_plot <- renderPlot({
      dat <- data.frame(score = cur_res()$Score)
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
      dat <- data.frame(date = as.Date(cur_res()$Date))
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
      paste(cur_res()$Species, collapse = "<br>"),
      "<br>")
    spp_tab <- sort(table(spp_list), decreasing = TRUE)[1:10]
    spp_df <- data.frame(species = names(spp_tab),
                         count = as.vector(spp_tab),
                         stringsAsFactors = FALSE)
    p <- ggplot(spp_df, aes(x = species, y = count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", subtitle = "Top 10 species") +
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
