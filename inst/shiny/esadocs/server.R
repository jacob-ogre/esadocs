# BSD_2_clause

result_asdf <- function(res) {
  score_ls <- vector("list", length(res))
  res_ls <- vector("list", length(res))
  id_ls <- vector("list", length(res))
  for(i in 1:length(res)) {     # NOTE: lapply doesn't work for some reason
    # print((res[[i]]$`_source`))
    score_ls[[i]] <- res[[i]]$`_score`
    id_ls[[i]] <- res[[i]]$`_id`
    type <- get_var(res[[i]]$`_source`, "type")
    title <- get_var(res[[i]]$`_source`, "title")
    date <- get_var(res[[i]]$`_source`, "date")
    file_name <- get_var(res[[i]]$`_source`, "file_name")
    link <- get_var(res[[i]]$`_source`, "link")
    pdf_path <- get_var(res[[i]]$`_source`, "pdf_path")
    txt_path <- get_var(res[[i]]$`_source`, "txt_path")
    # raw_text <- get_var(res[[i]]$`_source`, "raw_txt")
    pdf_md5 <- get_var(res[[i]]$`_source`, "pdf_md5")
    n_pages <- get_var(res[[i]]$`_source`, "n_pages")
    fr_citation_page <-  get_var(res[[i]]$`_source`, "fr_citation_page")
    plan_act_status <-  get_var(res[[i]]$`_source`, "plan_act_status")
    plan_status <-  get_var(res[[i]]$`_source`, "plan_status")
    federal_agency <- get_var(res[[i]]$`_source`, "federal_agency")
    activity_code <-  get_var(res[[i]]$`_source`, "activity_code")
    ch_status <- get_var(res[[i]]$`_source`, "ch_status")
    doc_type <- get_var(res[[i]]$`_source`, "doc_type")
    services <- get_var(res[[i]]$`_source`, "services")
    spp_tmp <- paste(res[[i]]$`_source`$species[[1]], collapse = "<br>")
    geo <- paste(res[[i]]$`_source`$geo[[1]], collapse = "<br>")
    tags <- paste(res[[i]]$`_source`$tags[[1]], collapse = "<br>")
    cur_dat <- data.frame(type = type,
                          title = title,
                          date = date,
                          file_name = file_name,
                          link = link,
                          pdf_path = pdf_path,
                          txt_path = txt_path,
                          # raw_txt = raw_text,
                          pdf_md5 = pdf_md5,
                          n_pages = n_pages,
                          fr_citation_page = fr_citation_page,
                          federal_agency = federal_agency,
                          activity_code = activity_code,
                          ch_status = ch_status,
                          doc_type = doc_type,
                          species = spp_tmp,
                          geo = geo,
                          tags = tags,
                          stringsAsFactors = FALSE)
    # names(cur_dat)[7] <- "raw_txt"
    res_ls[[i]] <- cur_dat
  }
  res_df <- suppressWarnings(dplyr::bind_rows(res_ls))
  res_df$score <- unlist(score_ls)
  res_df$id <- unlist(id_ls)
  return(res_df)
}

get_var <- function(src, varname) {
  ifelse(is.null(src[[varname]]),
         NA, src[[varname]])
}

get_highlight <- function(res) {
  res_ls = vector("list", length(res))

  # unfortunately, lapply doesn't seem to work well over an ES result object,
  # at least not without getting way more complicated than using a for loop
  for(i in 1:length(res)) {
    hi_tmp <- lapply(res[[i]]$highlight, FUN = abbrev)
    hi_tmp <- str_replace_all(hi_tmp,
                              "[ ]{2,}|\n",
                              " ")
    res_ls[[i]] <- hi_tmp
  }
  res_vec <- unlist(res_ls)
  return(res_vec)
}

abbrev <- function(x) {
  if(length(x) > 3) {
    paste(x[1:3], collapse = " ... ")
  } else {
    paste(x, collapse = " ... ")
  }
}

test_nulls <- function(x) {
  if(class(x) == "NULL" |
     class(x) == "NA" |
     class(x) == "logical") {
    return(TRUE)
  }
  return(FALSE)
}

###############################################################################
# SERVER

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
      return("<span style='color:red'>The server is down; please wait a few
             minutes and try again. If the problem persists,
             <a href='mailto:esa@defenders.org'>contact us</a>.")
    }
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

  min_score <- reactive({
    as.numeric(input$min_score)
  })

  max_hits <- reactive({
    as.numeric(input$max_hits)
  })

  # placeholder function for cleaning input; will probably be extended
  replace_chars <- function(x) {
    x <- gsub(x, pattern = '\'|\"', replacement = '\\"', fixed = TRUE)
    return(x)
  }

  # reactive form of input$main_input
  cur_input <- reactive({
    res <- replace_chars(input$main_input)
    return(res)
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
    if(grepl(cur_input(), pattern = "^(\"|\')[[:print:]]+(\"|\')$")) {
      body <- list(
                min_score = min_score(),
                `_source` = list(
                  excludes = "raw_txt"
                ),
                query = list(
                  match_phrase = list(
                    raw_txt.shingles = cur_input()
                  )
                ),
                size = max_hits(),
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
    } else {
      body <- list(
                min_score = min_score(),
                `_source` = list(
                  excludes = "raw_txt"
                ),
                query = list(
                  match = list(
                    raw_txt.shingles = cur_input()
                  )
                ),
                size = max_hits(),
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
    }
    # fgh <- index_clear_cache("esadocs")
    cur_mats <- Search("esadocs",
                       type = cur_type(),
                       body = body)$hits$hits
    if(length(cur_mats) > 0) {
      res_df <- result_asdf(cur_mats)
      res_df$highlight <- get_highlight(cur_mats)
      res_df <- distinct(res_df, file_name, .keep_all = TRUE)
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
    # observe(print(data$species[i]))
    # observe(print(data$geo[i]))
    div(id = paste0("pg", pg),
      div(class = "search-res",
        br(),
        fluidRow(
          column(10,
            a(href = ifelse(!is.na(data$pdf_path[i]),
                            data$pdf_path[i],
                            data$link[i]),
              target = "_blank",
              span(
                if(!is.null(data$title[i])) {
                  ifelse(!is.na(data$title[i]) & nchar(data$title[i]) > 10,
                         data$title[i],
                         data$file_name[i])
                } else {
                  data$file_name[i]
                },
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
              column(3,
                div(class = "info-div-right",
                  if(is.na(data$link[i])) {
                    "No original online"
                  } else {
                    span(icon("external-link"),
                         a(href = data$link[i],
                         target = "_blank",
                         "Original online"))
                  }
                )
              )
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
                  content = HTML(unlist(data$species[i])),
                  placement = "right",
                  trigger = "focus"
                )
              ),
              column(2,
                popify(
                  actionLink(
                    inputId = paste0("state", i),
                    label = div(style = "font-weight:300;",
                                "Geo-tags"),
                    style = "default"
                  ),
                  title = "Relevant geographic places",
                  content = HTML(unlist(data$geo[i])),
                  placement = "right",
                  trigger = "focus"
                )
              ),
              column(2,
                popify(
                  actionLink(
                    inputId = paste0("rel_agency", i),
                    label = div(style = "font-weight:300;",
                                "Agencies"),
                    style = "default"
                  ),
                  title = "Relevant government agencies",
                  content = data$federal_agency[i],
                  placement = "right",
                  trigger = "focus"
                )
              ),
              column(2,
                popify(
                  actionLink(
                    inputId = paste0("tags", i),
                    label = div(style = "font-weight:300;",
                                "Tags"),
                    style = "default"
                  ),
                  title = "Tags",
                  content = data$tags[i],
                  placement = "right",
                  trigger = "focus"
                )
              ),
              column(4,
                tags$div(
                  style="font-size:small; text-align:right; color:#bfbfbf",
                  data$id[i]
                )
              )
            )
          ),
          column(2)
        ),
        div(style = "background:rgba(0,0,0,0)",
          br()
        )
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

    observe(print(table(cur_res()$type)))

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

    output$pages_plot <- renderPlot({
      dat <- data.frame(score = cur_res()$n_pages)
      if(dim(dat)[1] == 0) return(NULL)
      nbin <- floor(dim(dat)[1] / 3)
      if(nbin < 1) nbin <- 1
      p <- ggplot(data = dat, aes(score)) +
             geom_histogram(bins = nbin) +
             labs(x = "# pages",
                  y = "# documents") +
             theme_hc()
      return(p)
    })

    output$date_plot <- renderPlot({
      dat <- data.frame(date = as.Date(cur_res()$date))
      output$n_na_date <- renderText({
        paste("# NA:", sum(is.na(dat$date)) )
      })
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

    output$spp_table <- DT::renderDataTable({
      spp_list <- str_split(paste(cur_res()$species, collapse = "<br>"), "<br>")
      spp_tab <- sort(table(spp_list), decreasing = TRUE)
      spp_df <- data.frame(species = names(spp_tab),
                           count = as.vector(spp_tab),
                           stringsAsFactors = FALSE)
      return(spp_df)
    })

    output$doc_types <- DT::renderDataTable({
      doc_tab <- sort(table(cur_res()$type), decreasing = TRUE)
      doc_df <- data.frame(type = names(doc_tab),
                           count = as.vector(doc_tab),
                           stringsAsFactors = FALSE)
      return(doc_df)
    })

    tabBox(
      tabPanel(
        title = "Date",
        br(),
        p(style="font-size:larger", "Document dates"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        plotOutput("date_plot", height = "350px"),
        helpText(textOutput("n_na_date"))
      ),
      tabPanel(
        title = "Species",
        br(),
        p(style="font-size:larger", "Species (taxa) in documents"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        DT::dataTableOutput("spp_table", height = "250px")
      ),
      tabPanel(
        title = "Type",
        br(),
        p(style="font-size:larger", "Types of documents"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        DT::dataTableOutput("doc_types", height = "250px")
      ),
      tabPanel(
        title = "Score",
        br(),
        p(style="font-size:larger", "Document scores given search terms"),
        hr(style="border-top-color:#d9d9d9 !important; border-top-width:2px"),
        plotOutput("score_plot", height = "350px")
      ),
      width = 12,
      height = "200px"
    )
  })

})
