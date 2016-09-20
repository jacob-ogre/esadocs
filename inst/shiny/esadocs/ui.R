# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
# header <- dashboardHeader(title = "ESAdocs",
#                           titleWidth = "10%")
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(
  div(class = "outer",
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
          top: 50px;
          left: 0;
          right: 0;
          bottom: 0;
          overflow: hidden;
          padding: 0;
        }

        /* Customize fonts */
        body, label, input, button, select {
          font-family: 'Open Sans';
          font-weight: 300;
        }
        h1, h2, h3, h4 { font-weight: 400; }

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
            div(h3("ESAdocs", style = "font-weight:bold;padding-bottom:2px"),
                span("Search", style = "font-size:125%;padding-top:5px"))
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
            helpText("Use ';' to separate fields for targeted searches.")
          ),
          column(2)
        ),

        br(), hr(), br(),

        fluidRow(
          column(8,
            div(style = "font-size:150%;font-weight:bold;padding-bottom:10px",
                textOutput("n_hits")),
            uiOutput("hits")
          ),
          column(4,
            helpText(textOutput("n_docs"))
          )
        )
      ),
      column(1)
    )
  )
)

dashboardPage(header, sidebar, body, skin="blue")
