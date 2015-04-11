basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source(file.path(basedir,"scripts","csv-to-html.R"))

power.raw <- file.path(basedir,"raw","Powers-raw.csv")
power.tag <- file.path(basedir,"raw","Powers-tags.csv")
power.lesser.raw <- file.path(basedir,"raw","Powers-Lesser-raw.csv")
power.lesser.tag <- file.path(basedir,"raw","Powers-Lesser-tags.csv")
css.file <- file.path(basedir,"raw","SFRPG.css")
power.htm.file <- file.path(basedir,"html","CharacterCreation","Powers.html")
power.table.htm.file <- file.path(basedir,"html","CharacterCreation","Powers-table.html")
power.lesser.htm.file <- file.path(basedir,"html","CharacterCreation","Powers-lesser.html")
power.lesser.table.htm.file <- file.path(basedir,"html","CharacterCreation","Powers-lesser-table.html")

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

power.raw.df <- read.csv(power.raw, sep=";")%>% tbl_df()  #%>% arrange(Level,Category, Keywords, Name) %>% filter(Name!="")
power.raw.df <- gsubColwise(power.raw.df,"\\n","<br>")


power.tag.df <- read.csv(power.tag, sep=";")
power.tag.pre<- power.tag.df[1,]
power.tag.post<- power.tag.df[2,]


css <- readChar(css.file, file.info(css.file)$size)


#Build power tables





power.table.htm<-buildTableApply(power.raw.df, 
                      df.names=c("Name", "Class", "Level", "UsageLimit","isAttack","Range","Action","Summary"),
                      tableClass="power-table")









# power.table.htm<-llply(power.raw.df %>%  
#                          group_by(Class) #%>% arrange(Level,Category) %>%
#                        #split(paste(power.raw.df$Level/100,power.raw.df$Category))  ,
#                        ,
#                        .fun=buildTableApply,
#                        df.names=setdiff(names(power.raw.df),"Text"),
#                        tableClass="power-table")
# 
# 
# 
# power.table.htm<-paste(power.table.htm,collapse="<br> ")


write(power.table.htm,power.table.htm.file)


#Build full text descriptions
power.htm<-buildElementApply(power.raw.df %>% arrange(Name), power.tag.pre, power.tag.post, df.names=setdiff(names(power.raw.df),"Summary"))


power.full <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   power.table.htm,
                   "<div class=\"power-List\">",
                   power.htm,
                   "</div>",
                   "</body></html>",
                   sep="\r\n",
                   collapse="")


writeChar(power.full,power.htm.file)

