basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source(file.path(basedir,"scripts","csv-to-html.R"))

feat.raw <- file.path(basedir,"raw","Feats-raw.csv")
feat.tag <- file.path(basedir,"raw","Feats-tags.csv")
feat.lesser.raw <- file.path(basedir,"raw","Feats-Lesser-raw.csv")
feat.lesser.tag <- file.path(basedir,"raw","Feats-Lesser-tags.csv")
css.file <- file.path(basedir,"raw","SFRPG.css")
feat.htm.file <- file.path(basedir,"html","CharacterCreation","Feats.html")
feat.table.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-table.html")
feat.lesser.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-lesser.html")
feat.lesser.table.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-lesser-table.html")

# No longer used
pasteRequirements <-function(req, level)
{#TODO: VECTORIZE!
  tmp <- as.character(req)
  emptyReqIndex <- level>1 & req==""
  fullReqIndex  <- level>1 & req!=""
  tmp[emptyReqIndex] <- paste("Level",level[emptyReqIndex])
  tmp[fullReqIndex] <- paste(req[fullReqIndex],", Level",level[fullReqIndex])
  as.factor(tmp)
}

feat.raw.df <- read.csv(feat.raw, sep=";")%>% tbl_df()  %>% arrange(Level,Category, Keywords, Name) %>% filter(Name!="")
feat.raw.df <- gsubColwise(feat.raw.df,"\\n","<br>")

feat.lesser.raw.df <- read.csv(feat.lesser.raw, sep=";")%>% tbl_df()  %>% arrange(Level,Keywords, Name) %>% filter(Name!="")
feat.lesser.raw.df <- gsubColwise(feat.lesser.raw.df,"\\n","<br>")

#Changed, level is now showed in column.
#feat.raw.df <- feat.raw.df %>% mutate(Requirements=pasteRequirements((Requirements),Level)) %>% select(-Level)


feat.tag.df <- read.csv(feat.tag, sep=";")
feat.tag.pre<- feat.tag.df[1,]
feat.tag.post<- feat.tag.df[2,]

feat.lesser.tag.df <- read.csv(feat.lesser.tag, sep=";")
feat.lesser.tag.pre<- feat.lesser.tag.df[1,]
feat.lesser.tag.post<- feat.lesser.tag.df[2,]

css <- readChar(css.file, file.info(css.file)$size)


#Build feat tables
rolesIndices <-grepl("Role",feat.raw.df$Keywords)
lesserIndices <-grepl("Lesser",feat.raw.df$Category)

feat.table.roles <- feat.raw.df %>% filter (rolesIndices)
# feat.table.lesser <- feat.raw.df %>% filter (lesserIndices)

feat.table.rest <- feat.raw.df %>% filter (!rolesIndices & !lesserIndices) %>%   refactor()

feat.table.roles.htm<-buildTableApply(feat.table.roles, 
                      df.names=setdiff(names(feat.raw.df),"Text"),
                      tableClass="Feat-table")

# feat.table.lesser.htm<-buildTableApply(feat.table.lesser, 
#                                       df.names=setdiff(names(feat.raw.df),"Text"),
#                                       tableClass="Feat-table")


feat.lesser.table.htm<-buildTableApply(feat.lesser.raw.df, 
                                       df.names=setdiff(names(feat.lesser.raw.df),"Text"),
                                       tableClass="Feat-table")


feat.table.rest.htm<-llply(feat.table.rest %>%  
                        group_by(Category) %>% 
                        split(feat.table.rest$Category)  ,
                      .fun=buildTableApply,
                      df.names=setdiff(names(feat.table.rest),"Text"),
                      tableClass="Feat-table")

feat.lesser.table.htm<-llply(feat.lesser.raw.df %>%  
                             group_by(Keywords) %>% 
                             split(feat.lesser.raw.df$Keywords)  ,
                           .fun=buildTableApply,
                           df.names=setdiff(names(feat.lesser.raw.df),"Text"),
                           tableClass="Feat-table")


feat.table.rest.htm<-c(feat.table.rest.htm[-which(names(feat.table.rest.htm)=="Epic Might")],feat.table.rest.htm["Epic Might"])
feat.table.rest.htm<-c(feat.table.rest.htm["Toughness"],feat.table.rest.htm[-which(names(feat.table.rest.htm)=="Toughness")])


feat.table.htm <- c(feat.table.rest.htm, Roles=feat.table.roles.htm)


feat.table.htm<-paste(feat.table.htm,collapse="<br> ")
feat.lesser.table.htm <- paste(feat.lesser.table.htm,collapse="<br> ")

write(feat.table.htm,feat.table.htm.file)
write(feat.lesser.table.htm,feat.lesser.table.htm.file)

#Build full text descriptions
feat.htm<-buildElementApply(feat.raw.df %>% arrange(Name), feat.tag.pre, feat.tag.post, df.names=setdiff(names(feat.raw.df),"Summary"))
feat.lesser.htm<-buildElementApply(feat.lesser.raw.df %>% arrange(Name), feat.lesser.tag.pre, feat.lesser.tag.post, df.names=setdiff(names(feat.lesser.raw.df),"Summary"))


feat.full <- paste("<html>\r\n<head>\r\n<title>Feat-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   feat.table.htm,
                   "<div class=\"Feat-List\">",
                   feat.htm,
                   "</div>",
                   "</body></html>",
                   sep="\r\n",
                   collapse="")

feat.lesser.full <- paste("<html>\r\n<head>\r\n<title>Feat-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   feat.lesser.table.htm,
                   "<div class=\"Feat-List\">",
                   feat.lesser.htm,
                   "</div>",
                   "</body></html>",
                   sep="\r\n",
                   collapse="")

writeChar(feat.full,feat.htm.file)
writeChar(feat.lesser.full,feat.lesser.htm.file)
