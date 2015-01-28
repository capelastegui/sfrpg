basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source(file.path(basedir,"scripts","csv-to-html.R"))

feat.raw <- file.path(basedir,"raw","Feats-raw.csv")
feat.tag <- file.path(basedir,"raw","Feats-tags.csv")
css.file <- file.path(basedir,"raw","SFRPG.css")
feat.htm.file <- file.path(basedir,"html","CharacterCreation","Feats.html")
feat.table.htm.file <- file.path(basedir,"html","CharacterCreation","Feats-table.html")
 
pasteRequirements <-function(req, level)
{#TODO: VECTORIZE!
  
  if (level==1) {tmp <-req}  else
  if (req=="") {tmp<-paste("Level",level)}else
  {tmp<-paste (req,", Level", level)}
  tmp
}

pasteRequirements <-function(req, level)
{#TODO: VECTORIZE!
  tmp <- req
  emptyReqIndex <- level>1 & req==""
  fullReqIndex  <- level>1 & req!=""
  tmp[emptyReqIndex] <- paste("Level",level[emptyReqIndex])
  tmp[fullReqIndex] <- paste(req[fullReqIndex],", Level",level[fullReqIndex])
  tmp                            
}



feat.raw.df <- read.csv(feat.raw, sep=";")%.% tbl_df() %.% group_by(Category) %.% arrange(Level,Category, Keywords, Name) %.% filter(Name!="")
feat.raw.df <- gsubColwise(feat.raw.df,"\\n","<br>")
feat.raw.df <- feat.raw.df %.% mutate(Requirements=pasteRequirements(as.character(Requirements),Level)) %.% select(-Level)

feat.tag.df <- read.csv(feat.tag, sep=";")


feat.tag.pre<- feat.tag.df[1,]
feat.tag.post<- feat.tag.df[2,]

css <- readChar(css.file, file.info(css.file)$size)

#quickStr(feat.raw.df)
#paste(feat.tag.pre$Name, feat.raw.df$Name[1], feat.tag.post$Name, sep="")

#Use these lines to test scripts
#tmp1<-buildElement(feat.raw.table$Name, feat.pre.table$Name, feat.post.table$Name)
#feat.raw.df<-feat.raw.df[1:3,]

feat.htm<-buildElementApply(feat.raw.df, feat.tag.pre, feat.tag.post, df.names=setdiff(names(feat.raw.df),"Summary"))
#write(feat.htm,feat.htm.file)


feat.table.htm<-buildTableApply(feat.raw.df, 
                                df.names=setdiff(names(feat.raw.df),"Text"),
                                tableClass="Feat-table")
write(feat.table.htm,feat.table.htm.file)

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

writeChar(feat.full,feat.htm.file)
