# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
# header <- dashboardHeader(title = "ESAdocs",
#                           titleWidth = "10%")
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(fluidPage(
  div(class = "outer",
    shinyjs::useShinyjs(),
    tags$head(
      HTML("<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400'
           rel='stylesheet' type='text/css'>"),
      tags$style(HTML("
        hr {
          border-color: #808080 !important;
          height: 2px;
        }

        .box {
          box-shadow: 2px 2px 2px #d9d9d9;
        }

        div.outer {
          background-color: white;
          top: 50px;
          left: 0;
          right: 0;
          bottom: 0;
          padding: 0;
        }

        /* Customize fonts */
        body, label, input, button, select {
          font-family: 'Open Sans';
          font-weight: 300;
        }
        h1, h2, h3, h4 { font-weight: 400; }

        .search-res {
          background-color:white;
          border-bottom: 1px solid #cccccc;
        }

        .selectize-input {
          font-size: small;
          border-color: white;
        }

        .selectize-dropdown {
          font-size: small;
        }

        img {
          display: block;
          margin: auto;
        }

        ")
      )
    ),

    bsModal(id = "a_modal",
            title = "Title",
            trigger = "contrib",
            p("Text..."),
            size = "large"
    ),

    br(),
    fluidRow(
      column(1),
      column(10,
        fluidRow(
          column(2,
            img(src = "ESAdocs_search.svg",
                height = "80px")
          ),
          column(8,
            div(class = "input-group", style = "padding-top:20px",
                textInput(inputId = "main_input",
                          label = NULL,
                          placeholder = "Search ESA documents",
                          width = "100%"),
                span(class = "input-group-btn",
                     bsButton(inputId = "search",
                              label = NULL,
                              icon = icon("search"),
                              style = "primary",
                              size = "default"
                     )
                )
            ),
            helpText(textOutput("n_docs"), style="font-size:smaller")
          ),
          column(2,
            img(src = "DOW_logo_small.png")
          )
        ),
        fluidRow(
          column(2,
            div(class = "slim",
              selectInput("show_n",
                          label = NULL,
                          choices = c("Hits per page", 10, 20, 50),
                          width = "75%",
                          multiple = FALSE)
            )
          )
        ),

        hr(style = "padding-above:2px; margin:1px"),

        fluidRow(
          column(8,
            helpText(textOutput("n_hits")),
            hidden(uiOutput("hits"))
          ),
          column(4,
            fluidRow(
              column(12,
                shinyjs::hidden(div(id = "selector",
                  radioButtons("summary_plot",
                               label = "Summarize by:",
                               choices = c("Score", "Date"),
                               inline = TRUE,
                               width = "75%"
                  ))
                ),
                plotOutput("score_dist", height="300px")
              )
            )
          )
        ),
        fluidRow(
          column(8,
            div(style = "width:50%; margin:0 auto;",
              div(style = "display: inline-block",
                hidden(bsButton("prevButton",
                         label = "< Previous",
                         style = "default",
                         size = "small"))
              ),
              hidden(div(id = "res_txt",
                         "Results pages",
                         style = "font-weight:bold; display:inline-block")),
              div(style = "display: inline-block",
                hidden(bsButton("nextButton",
                         label = "Next >",
                         style = "default",
                         size = "small"))
              )
            )
          ),
          column(4)
        ),
        fluidRow(
          br()
        )
      ),
      column(1)
    )
  )
))

# dashboardPage(header, sidebar, body, skin="blue")
body
