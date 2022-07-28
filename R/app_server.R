#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  league = "Sentinel"
  unique_prices <- get_df_of_unique_prices(league)
  showNotification(ui = "Initialization Complete", duration = 4)


  build_items <- reactive({
    req(input$start)
    input$pastebin_link %>% get_xml_from_link() %>%
    make_item_data_frame()
    })

  output$build_items <- DT::renderDT({
    req(input$start)
    left_join(x = build_items(), y = unique_prices,
      by = c("name", "base" = "baseType")) %>%
      select(!c("item_ids", "detailsId"))
    })

  # output$price_table <- DT::renderDT({
  #   req(input$start)
  #   unique_prices %>% select(!icon)
  # })


}
