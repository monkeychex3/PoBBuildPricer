#' helpers
#'
#' @description This takes in a pastebin link and parses it into xml
#'
#' @param link a pastebin link without "raw" in it
#'
#' @return xml to be read by xml2 functions
#'
#' @noRd

get_xml_from_link <- function(link){
  raw <- RCurl::getURL(link) %>% rvest::read_html() %>%
    rvest::html_element(xpath =
        "/html/body/div[1]/div[2]/div[1]/div[2]/div[3]/div[2]/ol/li/div") %>%
    rvest::html_text2()
  decompressed <- raw %>% gsub("-", "+", .) %>% gsub("_", "/", .) %>%
    RCurl::base64Decode(mode = "raw") %>% memDecompress(type = "gzip")
  xml <- decompressed %>% rawToChar() %>% xml2::read_xml()
  return(xml)
}


#' @description This takes in xml about a PoB build and turns
#' it into a dataframe of items
#'
#' @param top_level_xml essentially the same thing as a save file from PoB
#'
#' @return a dataframe of items from the build
#'
#' @noRd

make_item_data_frame <- function(top_level_xml){
  jewels_in_use <- top_level_xml %>% xml2::xml_find_first("Tree") %>%
    xml2::xml_find_first("Spec") %>% xml2::xml_find_first("Sockets") %>%
    xml2::xml_find_all("Socket") %>% xml2::xml_attr("itemId")

  ids_in_use <- top_level_xml %>% xml2::xml_find_first("Items") %>%
    xml2::xml_find_all("Slot") %>%
    xml2::xml_attr("itemId") %>% c(jewels_in_use)

  #this grabs good info, but misses "id"
  item_df <- top_level_xml %>% xml2::xml_find_first("Items") %>%
    xml2::xml_text() %>% stringr::str_extract_all(
      pattern = "Rarity:[:space:].+\\n.+\\n.+\\n") %>%
    unlist() %>% stringr::str_split(pattern = "\\n", simplify = TRUE) %>%
    as.data.frame() %>% dplyr::select(
      "rarity" = V1, "name" = V2, base = "V3") %>%
    dplyr::mutate(
      rarity = gsub(pattern = "Rarity: ", replacement = "", x = rarity))

  # this makes an ordered collection of item Id numbers
  item_ids <- top_level_xml %>% xml2::xml_find_first("Items") %>%
    xml2::xml_find_all("Item") %>% xml2::xml_attr("id")

  item_df <- cbind(item_ids, item_df)
  item_df <- item_df %>% dplyr::mutate(
    in_use = dplyr::case_when(
      item_ids %in% ids_in_use ~ "yes",
      TRUE ~ "no"
    )
  )
  return(item_df)
}


#' @description This hits al of the poe ninja api links and gets an overview of
#' equippable uniques
#'
#' @param league Which league we are interested in the prices of
#'
#' @return a dataframe of all equippable unique items
#'
#' @noRd

get_df_of_unique_prices <- function(league){

  accumulated_responses <- data.frame(name = NULL, icon = NULL,
    baseType = NULL, chaosValue = NULL,
    exaltedValue = NULL, detailsId = NULL)

  overviews <- c("UniqueJewel", "UniqueFlask", "UniqueWeapon",
    "UniqueArmour", "UniqueAccessory")

  for(overview in overviews){
    response <- httr::GET(paste0(
      "https://poe.ninja/api/data/itemoverview?league=",
      league, "&type=", overview, "&language=en")) %>% httr::content()

    response_table <- response$lines %>% tibble::enframe() %>%
      dplyr::select(!name) %>% tidyr::unnest_wider(value) %>%
      dplyr::select(name, icon, baseType,
        chaosValue, exaltedValue, detailsId)

    accumulated_responses <- rbind(accumulated_responses, response_table)
  }

  return(accumulated_responses)
}
