#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # load_bar <- waiter::Waitress$new()
  # load_bar$show()
  league = "Sentinel"
  unique_prices <- get_df_of_unique_prices(league)
  showNotification(ui = "Initialization Complete", duration = 4)

  # observeEvent(eventExpr = input$start,
  #   handlerExpr = {
  #     req(input$pastebin_link)
  #     build_items <- input$pastebin_link %>% get_xml_from_link() %>%
  #       make_item_data_frame()
  #   }
  # )

  build_items <- reactive({
    req(input$start)
    input$pastebin_link %>% get_xml_from_link() %>%
    make_item_data_frame()
    })

  output$build_items <- DT::renderDT({
    req(input$start)
    build_items()
    })

  output$price_table <- DT::renderDT({
    req(input$start)
    unique_prices %>% select(!icon)
  })


}
