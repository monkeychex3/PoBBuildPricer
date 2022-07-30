#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import DT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  league = "Sentinel"
  unique_prices <- get_df_of_unique_prices(league)
  showNotification(ui = "Initialization Complete", duration = 4)


  build_items <- reactive({
    req(input$start)
    input$pastebin_link %>% get_xml_from_link() %>%
    make_item_data_frame() %>% left_join(y = unique_prices,
      by = c("name", "base" = "baseType")) %>%
      select(!c("item_ids", "detailsId"))
    })

  tableOut <- reactive({
    req(input$start)

    build_items() %>%
      filter(rarity %in% input$filtering, in_use %in% input$filtering) %>%
      select(!c(exaltedValue), "equipped" = in_use) %>%
      mutate(., bought = shinyInput(
        checkboxInput, len = nrow(.), id = "exclude_price",
        value = FALSE, width = "50%")
      ) %>%
      mutate(chaosValue = round(chaosValue)) %>%
      arrange(desc(chaosValue)) %>%
      DT::datatable(
        rownames = FALSE, escape = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0,1,2))),
        options = list(paging = FALSE, searching = FALSE, info = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,5)))))
  })

  output$build_items <- DT::renderDT({
    req(input$start)

    tableOut()
    })

}
