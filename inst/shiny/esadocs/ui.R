# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(
    # tags$head(
    #     HTML("<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400' rel='stylesheet' type='text/css'>"),
    #     includeCSS("www/custom_styles.css")
    # ),

    bsModal(id = "a_modal",
            title = "Title",
            trigger = "contrib",
            p("Text..."),
            size = "large"
    ),

    navbarPage("Defenders ESC",
      fluidRow(
        column(1),
        column(10,
          fluidRow(
            column(2,
              p("ESAdocs", style = "font-size:24pt;font-weight:bold;padding-bottom:0px"),
              p("Search", style = "color:gray;padding-top:0px; line-height:60%")
            ),
            column(8,
              div(style = "padding-top:20px",
                  textInput(inputId = "main_input",
                            label = NULL,
                            placeholder = "Search ESA documents",
                            width = "100%"
                  )
              )
            ),
            column(2,
              div(style = "padding-top:20px",
                bsButton(inputId = "search",
                         label = "Go",
                         style = "primary",
                         size = "default",
                         type = "action"
                )
              )
            )
          ),
          br(), hr(), br(),
          fluidRow(
            column(9,
              p("RESULTS")
            ),
            column(3,
              p("summary info")
            )
          )
        ),
        column(1)
      )

))
