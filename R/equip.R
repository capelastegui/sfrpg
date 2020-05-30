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
  df_item %>%
    dplyr::mutate(
      title1=paste0("<span class=\"", rarity_colors, " large\"><strong>",
                    name, '</strong></span>', '<br>'),
      title2=paste0("<span class=\"", rarity_colors, "\"><strong>",
                    paste0(rarity, ' ' , type, ' - ',
                           paste(level_h, level_p,  level_e, sep='/'),
                    '</strong></span>', '<br>')),
      p_title=paste0(title1, title2)
    ) %>% dplyr::select (-title1, -title2)
}

#' Write magic items data
#'
#' @param df_items_raw
#' @param df_items_tag
#'
#' @return
#' @export
clean_df_items <- function(df_items_raw=NULL, df_items_tag=NULL, to_html=TRUE) {

  l_color_map=c(black = "Common", darkgray = "Uncommon", gold = "Rare")

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
        {suppressWarnings( forcats::fct_relevel(.,c('black','darkgray','gold')))}
    ) %>%
    dplyr::arrange(type, rarity_colors, level_h, name)

    if(!to_html) {
    return(df_items)
  }

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
    gsub_colwise("\\n","<br>") %>%  # Replace line breaks in raw file with html br
    collapse_cols('\r\n') %>%  # Add line breaks for html readability
    build_element('<div class="Power">','</div>') %>%
    paste(collapse='\r\n')

  add_strong<- function(str_in){
    paste0('<strong>',str_in, '</strong>')
  }


  htm_item <- paste0('<div class="Power-List">',item_block, '</div>') %>%
    # [H] is converted to link in markdown - need to escape
    # , also \\[H\\] is converted into something else
    stringr::str_replace_all('\\[', '&#91;') %>%
    stringr::str_replace_all('\\]', '&#93;') %>%
    stringr::str_replace_all("(Power.+?)\\:", add_strong) %>%
    stringr::str_replace_all("(Property|Special|Requirement)\\:", add_strong)

  htm_item
}

write_items_file <- function(htm_items, dir_output) {
  writeLines(htm_items, file.path(dir_output, 'magic_items.html'))
}