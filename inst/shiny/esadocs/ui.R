# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(fluidPage(
  div(class = "outer",
    shinyjs::useShinyjs(),
    tags$style(appCSS),
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
          border-bottom: 1px solid #f2f2f2;
          padding-bottom: 5px;
        }

        .selectize-input {
          border-color: white;
          font-size: small;
          margin-bottom: -10px;
        }

        .selectize-dropdown {
          font-size: small;
        }

        .btn-default, .sbs-toggle-button, .btn-xs {
          border-color: white;
        }

        .btn-default, .sbs-toggle-button, .action-button {
          border-radius: 1px;
          font-weight: bold;
        }

        .btn-default, .action-button:hover {
          border-radius: 1px;
          border-color: white;
        }

        /* DATE RANGE SELECTOR */
        .input-sm {
          background-color: #f2f2f2;
          border-color: white;
          color: #404040;
        }

        .input-group-addon {
          background-color: #404040;
          color: white;
        }
        /* DATE RANGE SELECTOR */

        .dropdown-toggle {
          border-color: white;
          font-size: small;
        }

        .dropdown-menu {
          font-size: small;
          padding-left: 10px;
          border-radius: 1px;
        }

        img {
          display: block;
          margin: auto;
        }

        .info-div {
          color: #009933;
          padding-top: 3px;
          padding-bottom: 3px;
        }

        .info-div-right {
          color: #009933;
          padding-top: 3px;
          padding-bottom: 3px;
          text-align: right;
        }

        .popover {
          border-radius: 1px;
          width: 350px;
          max-width: 350px;
        }

        .popover-title {
          font-size: 110%;
          font-weight: bold;
        }

        .popover-content {
          font-size: 90%;
          font-weight: light;
        }

        .nav-tabs>li>a, .nav-tabs>li.active {
          border-radius: 1px;
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
      column(1,
        tags$a(href="https://esadocs.cci-dev.org",
          img(src = "ESAdocs_search.svg",
              height = "80px")
        )
      ),
      column(10,
        fluidRow(
          column(1),
          column(9,
            fluidRow(
              div(
                class = "input-group",
                style = "padding-top:20px",
                textInput(
                  inputId = "main_input",
                  label = NULL,
                  placeholder = "Search ESA documents",
                  width = "100%"),
                span(
                  class = "input-group-btn",
                  withBusyIndicatorUI(
                    actionButton(
                      inputId = "search",
                      label = NULL,
                      icon = icon("search"),
                      style = "primary",
                      style="font-size:150%;
                             color:white;
                             background-color:#337ab7;
                             border-color:#2e6da4"
                    )
                  )
                )
              )
            ),
            fluidRow(
              helpText(htmlOutput("n_docs"))
            )
          ),
          column(2,
            div(style = "position: absolute; right:0px",
                tags$a(href="http://www.defenders.org",
                  img(src = "DOW_logo_small.png")
                )
            )
          )
        ),
        fluidRow(
          column(1,
            actionButton(
              inputId = "tog_extras",
              label = "Filters",
              # icon = icon("filter"),
              size = "large",
              type = "toggle",
              value = FALSE
            )
          ),
          hidden(
            div(id = "extras",
              column(2,
                div(class = "slim",
                  selectInput("show_n",
                              label = NULL,
                              choices = c("Hits per page (10)", 20, 50, 100),
                              width = "95%",
                              multiple = FALSE)
                )
              ),
              column(3,
                dateRangeInput(
                  "date_filt",
                  label = NULL,
                  start = as.Date("1967-01-01"),
                  end = Sys.Date(),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "type_filt",
                  label = NULL,
                  choices = list(
                    "All document types" = "all",
                    "Candidates" = "candidate",
                    "Conserv. Agreements" = "conserv_agmt",
                    "Consultation" = "consultation",
                    "Critical Habitat" = "critical_habitat",
                    "Federal Register" = "federal_register",
                    "Miscellaneous" = "misc",
                    "Policies" = "policy",
                    "Recovery Plan" = "recovery_plan",
                    "5-year review" = "five_year_review",
                    "7(a)(1)" = "section_7a1"
                  ),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "min_score",
                  label = NULL,
                  choices = list(
                    "Min score = 0.1" = 0.1,
                    "Min score = 0.5" = 0.5,
                    "Min score = 1" = 1,
                    "Min score = 5" = 5,
                    "No filter (0)" = 0
                  ),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "max_hits",
                  label = NULL,
                  choices = list(
                    "Max hits = 500" = 500,
                    "Max hits = 100" = 100,
                    "Max hits = 1000" = 1000,
                    "Max hits = 10000" = 10000
                  ),
                  width = "95%"
                ))
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
                br(),
                hidden(h4(id = "summ_head", "Supplemental results")),
                br(),
                uiOutput("summary_figs", height = "200px")
              )
            )
          )
        ),
        fluidRow(
          column(8,
            div(
              id = "nextprev",
              style = "width:50%; margin:0 auto;",
              div(style = "display: inline-block",
                hidden(actionButton("prevButton",
                         label = "< Previous",
                         style = "default",
                         size = "small"))
              ),
              hidden(div(id = "res_txt",
                         "Results pages",
                         style = "font-weight:bold; display:inline-block")),
              div(style = "display: inline-block",
                hidden(actionButton("nextButton",
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
