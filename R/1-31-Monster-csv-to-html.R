#source(file.path(here::here(),"R","0-00-csv-to-html.R"))

updateMonsters <- function() {

monster.race.raw <- file.path(here::here(),"raw","monsters","Monsters-race-raw.csv")
monster.race.tag <- file.path(here::here(),"raw","monsters","Monsters-race-tags.csv")
monster.class.raw <- file.path(here::here(),"raw","monsters","Monsters-class-raw.csv")
monster.class.tag <- file.path(here::here(),"raw","monsters","Monsters-class-tags.csv")
monster.css.file <- file.path(here::here(),"Rmd","SFRPG-monsters.css")
monster.race.htm.file <- file.path(here::here(),"html","monsters","Monsters.race.html")
monster.class.htm.file <- file.path(here::here(),"html","monsters","Monsters.class.html")
monster.htm.file <- file.path(here::here(),"html","monsters","Monsters.html")

monster.race.df <- read.csv(monster.race.raw, sep=",")
monster.race.tag.df <- read.csv(monster.race.tag, sep=";")
monster.race.tag.pre<- monster.race.tag.df[1,]
monster.race.tag.post<-monster.race.tag.df[2,]

monster.class.df <- read.csv(monster.class.raw, sep=",")
monster.class.tag.df <- read.csv(monster.class.tag, sep=";")
monster.class.tag.pre<- monster.class.tag.df[1,]
monster.class.tag.post<- monster.class.tag.df[2,]

monster.css <- readChar(monster.css.file, file.info(monster.css.file)$size)

#quickStr(feat.raw.df)
#paste(feat.tag.pre$Name, feat.raw.df$Name[1], feat.tag.post$Name, sep="")

#Use these lines to test R
#tmp1<-build_element(feat.raw.table$Name, feat.pre.table$Name, feat.post.table$Name)
monster.race.df<-monster.race.df[1:40,]
monster.race.df<-gsub_colwise(monster.race.df,"\\n", "<br>")
monster.class.df<-gsub_colwise(monster.class.df,"\\n", "<br>")

#monster.class.df<-monster.class.df[1,]

monster.race.htm<-build_element_apply(monster.race.df, monster.race.tag.pre, monster.race.tag.post, df.names=names(monster.race.df))
monster.class.htm<-build_element_apply(monster.class.df, monster.class.tag.pre, monster.class.tag.post, df.names=names(monster.class.df))

#write(feat.htm,feat.htm.file)

monster.race.full <- paste("<!DOCTYPE html>",
                      "<html>\r\n<head>\r\n<title>Monster stats</title>\r\n<style type=\"text/css\">",
                      monster.css,
                      "</style></head>\r\n<body>",
                      monster.race.htm,
                      "</body></html>",
                      sep="\r\n",
                      collapse="<p><br></p>")

monster.class.full <- paste("<!DOCTYPE html>",
                      "<html>\r\n<head>\r\n<title>Monster stats</title>\r\n<style type=\"text/css\">",
                      monster.css,
                      "</style></head>\r\n<body>",
                      monster.class.htm,
                      "</body></html>",
                      sep="\r\n",
                      collapse="<p><br></p>")

monster.full <- paste("<!DOCTYPE html>",
                      "<html>\r\n<head>\r\n<title>Monster stats</title>\r\n<style type=\"text/css\">",
                      monster.css,
                      "</style></head>\r\n<body>",
                      monster.race.htm,
                      monster.class.htm,
                      "</body></html>",
                      sep="\r\n",
                      collapse="<p><br></p>")

writeChar(monster.full,monster.htm.file)
writeChar(monster.race.full,monster.race.htm.file)
writeChar(monster.class.full,monster.class.htm.file)
}
