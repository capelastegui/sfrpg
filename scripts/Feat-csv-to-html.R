basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source(file.path(basedir,"scripts","csv-to-html.R"))

feat.raw <- file.path(basedir,"raw","charactercreation","Feats-raw.csv")
feat.tag <- file.path(basedir,"raw","charactercreation","Feats-tags.csv")
feat.lesser.raw <- file.path(basedir,"raw","charactercreation","Feats-Lesser-raw.csv")
feat.lesser.tag <- file.path(basedir,"raw","charactercreation","Feats-Lesser-tags.csv")
css.file <- file.path(basedir,"raw","SFRPG.css")
feat.htm.file <- file.path(basedir,"html","CharacterCreation","Feats.html")
feat.table.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-table.html")
feat.lesser.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-lesser.html")
feat.lesser.table.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-lesser-table.html")

feat.raw.df <- read.csv(feat.raw, sep=";")%>% tbl_df()  %>% arrange(Level,Category, Keywords, Name) %>% filter(Name!="")
feat.raw.df <- gsubColwise(feat.raw.df,"\\n","<br>")

feat.lesser.raw.df <- read.csv(feat.lesser.raw, sep=";")%>% tbl_df()  %>% arrange(Level,Keywords, Name) %>% filter(Name!="")
feat.lesser.raw.df <- gsubColwise(feat.lesser.raw.df,"\\n","<br>")

feat.tag.df <- read.csv(feat.tag, sep=";")
feat.tag.pre<- feat.tag.df[1,]
feat.tag.post<- feat.tag.df[2,]

feat.lesser.tag.df <- read.csv(feat.lesser.tag, sep=";")
feat.lesser.tag.pre<- feat.lesser.tag.df[1,]
feat.lesser.tag.post<- feat.lesser.tag.df[2,]

css <- readChar(css.file, file.info(css.file)$size)


#Build feat tables

# NO LONGER USEd: GENERATES A SINGLE MEGA-TABLE FOR FEATS
#feat.table.htm<-buildTableApply(feat.raw.df, df.names=setdiff(names(feat.raw.df),"Text") )
#feat.lesser.table.htm<-buildTableApply(feat.lesser.raw.df, df.names=setdiff(names(feat.lesser.raw.df),"Text"))

#note: divide by 100 included to fix level order (otherwise orders as string)

feat.table.htm<-llply(feat.raw.df %>%  
                        group_by(Level) %>% arrange(Level,Category) %>%
                        split(paste(feat.raw.df$Level/100,feat.raw.df$Category))  ,
                      .fun=buildTableApply,
                      df.names=setdiff(names(feat.raw.df),"Text"))

feat.lesser.table.htm<-llply(feat.lesser.raw.df %>%  
                             group_by(Keywords) %>% 
                             split(feat.lesser.raw.df$Keywords)  ,
                           .fun=buildTableApply,
                           df.names=setdiff(names(feat.lesser.raw.df),"Text"))

feat.table.htm<-paste(feat.table.htm,collapse="<br> ")
feat.lesser.table.htm <- paste(feat.lesser.table.htm,collapse="<br> ")

#Now unused, generates feat table files, keep for debug
#write(feat.table.htm,feat.table.htm.file)
#write(feat.lesser.table.htm,feat.lesser.table.htm.file)

#Build full text descriptions
feat.htm<-buildElementApply(feat.raw.df %>% arrange(Name), feat.tag.pre, feat.tag.post, df.names=setdiff(names(feat.raw.df),"Summary"))
feat.lesser.htm<-buildElementApply(feat.lesser.raw.df %>% arrange(Name), feat.lesser.tag.pre, feat.lesser.tag.post, df.names=setdiff(names(feat.lesser.raw.df),"Summary"))

feat.list  <- llply(feat.raw.df %>%  
                      group_by(Level) %>% arrange(Level,Name) %>%
                      split(feat.raw.df$Level)  ,
                    .fun=buildElementApply,
                    feat.tag.pre, feat.tag.post,
                    df.names=setdiff(names(feat.raw.df),"Text"),
                    skipEmpty=TRUE)



feat.full <- paste("<html>\r\n<head>\r\n<title>Feat-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   "<div class=\"Feat-Table\">",
                   feat.table.htm,
                   "</div>",
                   "<div class=\"Feat-List\">",
                   feat.htm,
                   "</div>",
                   "</body></html>",
                   sep="\r\n",
                   collapse="")

feat.lesser.full <- paste("<html>\r\n<head>\r\n<title>Feat-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   "<div class=\"Feat-Table\">",
                   feat.lesser.table.htm,
                   "</div>",
                   "<div class=\"Feat-List\">",
                   feat.lesser.htm,
                   "</div>",
                   "</body></html>",
                   sep="\r\n",
                   collapse="")

writeChar(feat.full,feat.htm.file)
writeChar(feat.lesser.full,feat.lesser.htm.file)
