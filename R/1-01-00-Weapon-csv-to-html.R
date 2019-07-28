
getEquipmentTables <- function (basedir=here::here())
{

source (file.path(basedir,"R","0-00-csv-to-html.R"))
css.file <- file.path(basedir,"Rmd","SFRPG.css")
weapons.table.htm.file <- file.path(basedir,"html","CharacterCreation","Weapons-table.html")
armor.table.htm.file <- file.path(basedir,"html","CharacterCreation","Armor-table.html")
armorbyclass.table.htm.file <- file.path(basedir,"html","CharacterCreation","Armor-by-class-legacy.html")
implement.table.htm.file <- file.path(basedir,"html","CharacterCreation","Implement-table.html")

css <- readChar(css.file, file.info(css.file)$size)
weapons.table <- read.csv(file.path(basedir,"raw","CharacterCreation","Weapons-raw.csv"), sep=";") 
armor.table <- read.csv(file.path(basedir,"raw","CharacterCreation","Armor-raw.csv"), sep=";")
implement.table <- read.csv(file.path(basedir,"raw","CharacterCreation","Implement-raw.csv"), sep=";")
names(armor.table) <- gsub("\\."," ",names(armor.table))
armorbyclass.table <- read.csv(file.path(basedir,"raw","CharacterCreation","Armor-by-class-legacy.csv"), sep=";")%>% arrange(Class)

#Build weapons tables




weapons.table.htm<-llply(weapons.table %>%  
                        split(weapons.table$Training)  ,
                      .fun=buildTableApply,
                      df.names=names(weapons.table),
                      tableClass="General-table")

armor.table.htm  <- buildTableApply(armor.table,tableClass="General-table")
armorbyclass.table.htm  <- buildTableApply(armorbyclass.table,tableClass="General-table")

implement.table.htm  <- buildTableApply(implement.table,tableClass="General-table")

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