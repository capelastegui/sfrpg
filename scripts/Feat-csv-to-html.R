basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source(file.path(basedir,"scripts","csv-to-html.R"))

feat.raw <- file.path(basedir,"raw","Feats-raw.csv")
feat.pre <- file.path(basedir,"raw","Feats-pre.csv")
feat.post <- file.path(basedir,"raw","Feats-post.csv")
feat.htm.file <- file.path(basedir,"html","Feats.html")
feat.table.htm.file <- file.path(basedir,"html","Feats-table.html")
 
feat.raw.table <- read.csv(feat.raw, sep=";")
feat.pre.table <- read.csv(feat.pre, sep=";")
feat.post.table <- read.csv(feat.post, sep=";")

#quickStr(feat.raw.table)
paste(feat.pre.table$Name, feat.raw.table$Name[1], feat.post.table$Name, sep="")

#TODO: change data frame from factors to characters
#tmp1<-buildElement(feat.raw.table$Name, feat.pre.table$Name, feat.post.table$Name)
feat.htm<-buildElementApply(feat.raw.table, feat.pre.table, feat.post.table, df.names=setdiff(names(feat.raw.table),"Summary"))
write(feat.htm,feat.htm.file)


feat.table.htm<-buildTableApply(feat.raw.table, df.names=setdiff(names(feat.raw.table),"Text"))
write(feat.table.htm,feat.table.htm.file)
