#' Generate html tables for equipment from source .csv files
#'
#'
#' @return List of strings with html tables
#' @export
get_df_equip <- function ()
{
  df_weapons <- read_my_csv("weapons")
  df_armor <- read_my_csv("armor") %>%
    purrr::set_names(names(.) %>% stringr::str_replace("\\."," "))
  df_implement <- read_my_csv("implements")
  df_armorbyclass <- read_my_csv('armor_by_class_reference') %>%
    dplyr::arrange(Class)
  
  #Build weapons tables
  
  df_weapons_str <- df_weapons %>%
    dplyr::group_by(table_type='Weapons', Training) %>% tidyr::nest() %>%
    dplyr::mutate(table = purrr::map_chr(data,build_table_apply, tableClass="General-table"))
  
  get_table <- function(df, type) {
    df %>% dplyr::group_by(table_type=type, Training='-') %>%  tidyr::nest() %>%
      dplyr::mutate(table = purrr::map_chr(data,build_table_apply, tableClass="General-table"))
  }
  
  df_armor_str  <- get_table(df_armor, 'Armor')
  df_armorbyclass_str  <- get_table(df_armorbyclass, 'Armor_by_Class')
  
  df_implement_str  <- get_table(df_implement, 'Implements')
  # dplyr::bind_rows raises annoying warnings - better to suppress them
  df_result = suppressWarnings(
      dplyr::bind_rows(
      df_weapons_str,
      df_armor_str,
      df_armorbyclass_str,
      df_implement_str
    )
  )
  df_result
}

#' Convenience function to extract subtable from equip_table
#'
#' @param df_tables_equip 
#' @param type 
#' @param subtype 
#'
#' @return String with an html table for weapons, implements, or armor.
#' @export
extract_equip_table <- function(df_tables_equip, type, subtype='-'){
  df_tables_equip %>%
    dplyr::filter(table_type==type, Training==subtype) %>%
    {.$table[1]}
}