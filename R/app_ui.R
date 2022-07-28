#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tags$head(
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(0%);
             left: calc(35%);
             }
             "
          )
        )
      ),
      titlePanel("Build Pricer (Poe 3.18)"),
      mainPanel(
        fluidRow(
          column(width = 6,
            textInput("pastebin_link", placeholder = "Pastebin Link", label = "Price Build")
          ),
          column(width = 2,
            br(),
            actionButton("start", label = "Get Price")
          ),
          column(width = 4,
            radioButtons("items_to_gather", label = "items of interest (just for show at the moment)",
            choices = c("all items", "equipped items", "unique items", "equipped uniques")
          ))
        ),
        fluidRow(
          DT::dataTableOutput("build_items")
        ),
        br()#,
        # h3("Price table below, Build items above"),
        # fluidRow(
        #   DT::dataTableOutput("price_table")
        # )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PoBBuildPricer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
