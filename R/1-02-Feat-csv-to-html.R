#require(rutils)

#auxiliary functions

refactor<-function(data){colwise(function(col) {if (is.factor(col)) factor(col)  else col }) (data)}

applysplit  <- function (df,splitvar)
{ 
  df <- refactor(df)
  split(df,df[[splitvar]])
}


getFullFeatHtm  <- function(featlist)
{
  featlist2  <- llply.name(featlist,.fun=function(l,l.name)
  {
    s  <- llply.name(l,.fun=function(l,l.name){paste0("<h4 class=\"newPage\">Category - ",l.name,"</h4>",
                                                      "<div class=\"Feat-table\">",l$table,"</div>",
                                                      "<div class=\"Feat-List\">", l$htm, "</div>")})
    l <- mapply(FUN=function(a,b){attr(a,"section") <- b;a},l,s, SIMPLIFY=FALSE)
    s <- paste0 (s,collapse="<br/>")
    attr(l,"full") <- paste0 ("<h3>Level ",l.name," feats<h3>",s)
    l
  }
  )
  attr(featlist2,"full") <- paste0(featlist2,collapse="<br/>\r\n")
  attr(featlist2,"full") <- paste0(llply(featlist2,.fun=attr,which="full"),collapse="<br/>\r\n")
  featlist2
}

#Generate feat table and feat list from a single raw feat table, and pre- and -post tags

getFeatSection  <- function(featraw, pre, post)
{
  feat.table  <- build_table_apply(featraw,df.names=setdiff(names(featraw),"Text"))
  feat.htm  <- build_element_apply(featraw,pre,post,df.names=setdiff(names(featraw),"Summary"))
  paste0 (feat.table,feat.htm,collapse="<br/>\r\n")
}


getFeatList <- function (basedir=here::here())
{
  source(file.path(basedir,"R","0-00-csv-to-html.R"))
  
  feat.raw <- file.path(basedir,"raw","charactercreation","Feats-raw.csv")
  feat.tag <- file.path(basedir,"raw","charactercreation","Feats-tags.csv")
  feat.lesser.raw <- file.path(basedir,"raw","charactercreation","Feats-Lesser-raw.csv")
  feat.lesser.tag <- file.path(basedir,"raw","charactercreation","Feats-Lesser-tags.csv")
  css.file <- file.path(basedir,"Rmd","SFRPG.css")
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
  
  
  feat.list.raw <- llply(split(feat.raw.df,feat.raw.df$Level),.fun=applysplit, splitvar="Category")
  feat.list.table  <- llply.2(feat.list.raw,.fun=build_table_apply,
                              df.names=setdiff(names(feat.raw.df),"Text"))
  feat.list.htm  <- llply.2(feat.list.raw,.fun=build_element_apply,
                            feat.tag.pre, feat.tag.post, df.names=setdiff(names(feat.raw.df),"Summary"))
  
  
  feat.lesser.list.raw  <- llply(split(feat.lesser.raw.df,feat.lesser.raw.df$Level),.fun=applysplit, splitvar="Keywords")
  feat.lesser.list.table  <- llply.2(feat.lesser.list.raw,.fun=build_table_apply,
                                     df.names=setdiff(names(feat.lesser.raw.df),"Text"))
  feat.lesser.list.htm  <- llply.2(feat.lesser.list.raw,.fun=build_element_apply,
                                   feat.lesser.tag.pre, feat.lesser.tag.post, df.names=setdiff(names(feat.lesser.raw.df),"Summary"))
  
  
  feat.list <- list()
  
  feat.list$feats <- 
    mapply(FUN=function(a,b,c)
    {
      mapply(a,b,c,SIMPLIFY=FALSE, FUN=function(a,b,c){list(table=a,htm=b,raw=c)})
    },
    feat.list.table,feat.list.htm,feat.list.raw, SIMPLIFY=FALSE)
  
  feat.list$feats  <- getFullFeatHtm(feat.list$feats)
  
  feat.list$lesserfeats <- 
    mapply(FUN=function(a,b)
    {
      mapply(a,b,SIMPLIFY=FALSE, FUN=function(a,b){list(table=a,htm=b)})
    },
    feat.lesser.list.table,feat.lesser.list.htm, SIMPLIFY=FALSE)
  
  feat.list$lesserfeats  <- getFullFeatHtm(feat.list$lesserfeats)
  
  feat.list$tag$pre <-feat.tag.pre
  feat.list$tag$post <-feat.tag.post
  
  feat.list
}









# paste.list.2  <- function (l,collapse=NULL)
# {
#   l <- llply(l, .fun=paste, collapse=collapse)
#   paste(l,collapse=collapse)
# }
# 
# 
# feat.table.htm <- paste.list.2(feat.list.table,collapse="<br>")
# feat.lesser.table.htm <- paste.list.2(feat.lesser.list.table,collapse="<br>")



# #Now unused, generates feat table files, keep for debug
#
# write(feat.table.htm,feat.table.htm.file)
# write(attr(genFeatSection(tmp),"full"),feat.table.htm.file)
# write(feat.lesser.table.htm,feat.lesser.table.htm.file)

#Build full text descriptions


# feat.full <- paste("<html>\r\n<head>\r\n<title>Feat-test</title>\r\n<style type=\"text/css\">",
#                    css,
#                    "</style></head>\r\n<body>",
#                    attr(genFeatSection(feat.list$feats),"full"),
#                    "</body></html>",
#                    sep="\r\n",
#                    collapse="")
# 
# feat.lesser.full <- paste("<html>\r\n<head>\r\n<title>Feat-test</title>\r\n<style type=\"text/css\">",
#                    css,
#                    "</style></head>\r\n<body>",
#                    attr(genFeatSection(feat.list$lesserfeats),"full"),
#                    "</body></html>",
#                    sep="\r\n",
#                    collapse="")
# 
# writeChar(feat.full,feat.htm.file)
# writeChar(feat.lesser.full,feat.lesser.htm.file)


#writeFeatFile()
