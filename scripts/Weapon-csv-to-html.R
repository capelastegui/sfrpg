basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source (file.path(basedir,"scripts","csv-to-html.R"))

css.file <- file.path(basedir,"raw","SFRPG.css")
css <- readChar(css.file, file.info(css.file)$size)
weapons.table.htm.file <- file.path(basedir,"html","CharacterCreation","Weapons-table.html")
armor.table.htm.file <- file.path(basedir,"html","CharacterCreation","Armor-table.html")
armorbyclass.table.htm.file <- file.path(basedir,"html","CharacterCreation","Armor-by-class-table.html")

weapons.table <- read.csv(file.path(basedir,"raw","Weapons-raw.csv"), sep=";")%>% tbl_df()
armor.table <- read.csv(file.path(basedir,"raw","Armor-raw.csv"), sep=";")%>% tbl_df()
armorbyclass.table <- read.csv(file.path(basedir,"raw","Armor-by-class.csv"), sep=";")%>% tbl_df() %>%
  arrange(Class)

#Build weapons tables




weapons.table.htm<-llply(weapons.table %>%  
                        group_by(Category) %>% 
                        split(weapons.table$Category)  ,
                      .fun=buildTableApply,
                      df.names=names(weapons.table),
                      tableClass="Feat-table")

armor.table.htm  <- buildTableApply(armor.table,tableClass="Feat-table")
armorbyclass.table.htm  <- buildTableApply(armorbyclass.table,tableClass="Feat-table")



weapons.table.htm<-paste(weapons.table.htm,collapse="<br> ")

weapons.table.htm <- paste("<html>\r\n<head>\r\n<title>Weapons</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   weapons.table.htm,
                   "</body></html>",
                   sep="\r\n",
                   collapse="")

armor.table.htm <- paste("<html>\r\n<head>\r\n<title>Armor</title>\r\n<style type=\"text/css\">",
                           css,
                           "</style></head>\r\n<body>",
                           armor.table.htm,
                           "</body></html>",
                           sep="\r\n",
                           collapse="")

armorbyclass.table.htm <- paste("<html>\r\n<head>\r\n<title>Armor</title>\r\n<style type=\"text/css\">",
                         css,
                         "</style></head>\r\n<body>",
                         armorbyclass.table.htm,
                         "</body></html>",
                         sep="\r\n",
                         collapse="")


write(weapons.table.htm,weapons.table.htm.file)
write(armor.table.htm,armor.table.htm.file)
write(armorbyclass.table.htm,armorbyclass.table.htm.file)

#exploration, delete this
ggplot(armorbyclass.table)+aes(x=Expected.AC,fill=Role)+geom_bar(binwidth=1,color="black")