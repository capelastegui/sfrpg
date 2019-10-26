#' Generate html tables for equipment from source .csv files
#'
#'
#' @return List of strings with html tables
#' @export
#'
#' @examples
#' df_tables_equip = get_equip_tables()
get_equip_tables <- function ()
{
  dir_base  = system.file('raw', "CharacterCreation", package='sfrpg')
  print(dir_base)
  read_my_csv <- function(s) {
    readr::read_delim(file.path(dir_base,paste0(s,".csv")),
                      delim=";")}
  
  df_weapons <- read_my_csv("Weapons-raw")
  df_armor <- read_my_csv("Armor-raw") %>%
    purrr::set_names(names(.) %>% stringr::str_replace("\\."," "))
  df_implement <- read_my_csv("Implement-raw")
  df_armorbyclass <- read_my_csv('Armor-by-class-legacy') %>%
    dplyr::arrange(Class)
  
  #Build weapons tables
  
  str_weapons<-df_weapons %>%
    split(df_weapons$Training) %>%
    purrr::map(build_table_apply, tableClass='General-table')

  # New workflow model: encapsulate data in nested dataframe
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
  
  df_result = dplyr::bind_rows(
    df_weapons_str,
    df_armor_str,
    df_armorbyclass_str,
    df_implement_str
  )
  df_result
}
