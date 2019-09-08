library(dplyr)
library(stringr)

getEquipmentTables <- function (basedir=here::here())
{
  file_utils = file.path(basedir,"R","0-00-csv-to-html.R")
  source (file_utils)
  file_css <- file.path(basedir,"Rmd","SFRPG.css")
  get_file_htm <- function(s) {file.path(basedir,"html","CharacterCreation",s)}
  file_weapons_table_html <- get_file_htm("Weapons-table.html")
  file_armor_table_html <- get_file_htm("Armor-table.html")
  file_armorbyclass_table_htm <- get_file_htm("Armor-by-class-legacy.html")
  file_implement_table_htm <- get_file_htm("Implement-table.html")
  
  css <- readChar(file_css, file.info(file_css)$size)
  read_my_csv <- function(s) {
      read.csv(file.path(basedir,"raw","CharacterCreation",paste0(s,".csv")),
      sep=";")}
  table_weapons <- read_my_csv("Weapons-raw")
  table_armor <- read_my_csv("Armor-raw") %>%
    purrr::set_names(names(.) %>% stringr::str_replace("\\."," "))

  table_implement <- read_my_csv("Implement-raw")
  table_armorbyclass <- read_my_csv('Armor-by-class-legacy') %>%
    dplyr::arrange(Class)

  #Build weapons tables

  weapons.table.htm<-llply(table_weapons %>%
                             split(table_weapons$Training)  ,
                           .fun=build_table_apply,
                           df.names=names(table_weapons),
                           tableClass="General-table")
  
  armor.table.htm  <- build_table_apply(table_armor,tableClass="General-table")
  armorbyclass.table.htm  <- build_table_apply(table_armorbyclass,tableClass="General-table")
  
  implement.table.htm  <- build_table_apply(table_implement,tableClass="General-table")
  
  equipList <- list(weapons=weapons.table.htm, 
                    armor=armor.table.htm, 
                    legacy.class.armor=armorbyclass.table.htm,
                    implements=implement.table.htm)
  equipList
}
# weapons.table.htm<-paste(weapons.table.htm,collapse="\r\n<br> ")
# 
# weapons.table.htm <- paste("<html>\r\n<head>\r\n<title>Weapons</title>\r\n<style type=\"text/css\">",
#                    css,
#                    "</style></head>\r\n<body>",
#                    weapons.table.htm,
#                    "</body></html>",
#                    sep="\r\n",
#                    collapse="")
# 
# armor.table.htm <- paste("<html>\r\n<head>\r\n<title>Armor</title>\r\n<style type=\"text/css\">",
#                            css,
#                            "</style></head>\r\n<body>",
#                            armor.table.htm,
#                            "</body></html>",
#                            sep="\r\n",
#                            collapse="")
# 
# armorbyclass.table.htm <- paste("<html>\r\n<head>\r\n<title>Armor</title>\r\n<style type=\"text/css\">",
#                          css,
#                          "</style></head>\r\n<body>",
#                          armorbyclass.table.htm,
#                          "</body></html>",
#                          sep="\r\n",
#                          collapse="")
# 
# implement.table.htm <- paste("<html>\r\n<head>\r\n<title>Armor</title>\r\n<style type=\"text/css\">",
#                          css,
#                          "</style></head>\r\n<body>",
#                          implement.table.htm,
#                          "</body></html>",
#                          sep="\r\n",
#                          collapse="")




# write(weapons.table.htm,weapons.table.htm.file)
# write(armor.table.htm,armor.table.htm.file)
# write(armorbyclass.table.htm,armorbyclass.table.htm.file)
# write(implement.table.htm,implement.table.htm.file)

#exploration, delete this.
#require(ggplot2)
#ggplot(armorbyclass.table)+aes(x=Expected.AC,fill=Role)+geom_bar(binwidth=1,color="black")
#ggplot(armorbyclass.table)+aes(x=Expected.AC,fill=Armor.Type)+geom_bar(binwidth=1,color="black")+facet_grid(Role ~.)