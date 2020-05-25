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


#' Add title paragraph column for item block
#' @export
add_p_item_title <- function(df_item, rarity_colors){
  p_title <- df_item %>%
    dplyr::select(name, type, rarity, level_h, level_p, level_e) %>%
    collapse_cols(' ')

  df_item %>%
    dplyr::mutate(p_title = paste0(
      "<span class=\"", rarity_colors, " large\"><strong>",
      p_title, '</strong></span>'))
}

#' Write magic items data
#'
#' @param df_items_raw
#' @param df_items_tag
#'
#' @return
#' @export
clean_df_items <- function(df_items_raw=NULL, df_items_tag=NULL) {

  l_color_map=c(black = "Common", silver = "Uncommon", gold = "Rare")

  if (df_items_raw %>% is.null())  {
    df_items_raw <- read_my_csv('magic_items')
  }

  if (df_items_tag %>% is.null())  {
    df_items_tag <- read_my_csv('item_tags')
  }

  df_items <- df_items_raw %>%
    fillna_df %>%
    dplyr::mutate(
      rarity_colors = rarity %>%
        factor %>%
        # Assign colors as factor levels
        # Noisy function, suppressing warnings
        #forcats::fct_recode(!!!l_color_map) %>%
        {suppressWarnings( forcats::fct_recode(.,!!!l_color_map))} %>%
        # Reorder factor levels: green,red,gray
        # Noisy function, suppressing warnings
        #forcats::fct_relevel(c('green','red','gray'))
        {suppressWarnings( forcats::fct_relevel(.,c('black','gray','gold')))}
    ) %>%
    dplyr::arrange(type, rarity, level_h, name)

  items_tag_pre <- df_items_tag[1,] #%>% dplyr::select(-Class,-Build)
  items_tag_post <- df_items_tag[2,] #%>% dplyr::select(-Class,-Build)

  build_cols <- setdiff(names(df_items),
    c("Summary", "Build", "usageColors"))

  item_block_tmp <- df_items %>%
    build_element_apply(items_tag_pre, items_tag_post,
      build_cols, reduce=FALSE) %>%
      add_p_item_title() %>%
      dplyr::select(p_title, p1, p2, p3, p4, p5) %>%
      purrr::map_dfc(add_p_tags)

  item_block <- item_block_tmp %>%
    collapse_cols('\r\n') %>%
    build_element('<div class="Power">','</div>') %>%
    paste(collapse='\r\n')

  htm_item <- paste0('<div class="Power-List">',item_block, '</div>')

  htm_item
}

write_items_file <- function(htm_items, dir_output) {
  writeLines(htm_items, file.path(dir_output, 'magic_items.html'))
}