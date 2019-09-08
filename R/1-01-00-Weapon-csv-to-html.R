
library(dplyr)
library(stringr)
library(readr)

#' Generate html tables for equipment from source .csv files
#'
#' @param dir_base Path of base project directory
#'
#' @return List of strings with html tables
#' @export
#'
#' @examples
#' l_tables_equip = get_equip_tables(dir_base)
get_equip_tables <- function (dir_base=here::here())
{
  file_utils = file.path(dir_base,"R","0-00-csv-to-html.R")
  source (file_utils)
  #get_file_htm <- function(s) {file.path(dir_base,"html","CharacterCreation",s)}

  read_my_csv <- function(s) {
      readr::read_delim(file.path(dir_base,"raw","CharacterCreation",paste0(s,".csv")),
                        delim=";")}

  df_weapons <- read_my_csv("Weapons-raw")
  df_armor <- read_my_csv("Armor-raw") %>%
    purrr::set_names(names(.) %>% stringr::str_replace("\\."," "))
  df_implement <- read_my_csv("Implement-raw")
  df_armor <- read_my_csv('Armor-by-class-legacy') %>%
    dplyr::arrange(Class)

  #Build weapons tables

  str_weapons<-df_weapons %>%
    split(df_weapons$Training) %>%
    purrr::map(build_table_apply, tableClass='General-table')
  
  str_armor  <- build_table_apply(df_armor,tableClass="General-table")
  str_armorbyclass  <- build_table_apply(df_armor,tableClass="General-table")
  
  str_implement  <- build_table_apply(df_implement,tableClass="General-table")
  
  equipList <- list(weapons=str_weapons,
                    armor=str_armor,
                    legacy.class.armor=str_armorbyclass,
                    implements=str_implement)
  equipList
}