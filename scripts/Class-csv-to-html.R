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

class.stat.raw <- file.path(basedir,"raw","Class-stats.csv")
class.stat.htm.file  <- file.path(basedir,"html","CharacterCreation","Class-stats.html")

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

class.stat.df  <- read.csv(class.stat.raw, sep=";", header=FALSE)#%>% tbl_df()  
#NOTE: using tbl_df() has weird interactions with stuff like as.character(class.stat.df[,1])
#names(class.stat.df)  <- paste(class.stat.df[,1],class.stat.df[,2],sep=".")
class.stat.df  <- cbind(V0=paste(class.stat.df[,1],class.stat.df[,2],sep="."),class.stat.df)
class.stat.df <- trans_df(class.stat.df) %>% tbl_df()

class.stat.list <- llply(class.stat.df[2:length(class.stat.df)],
                         class.stat.df[,1],.fun=function(a,b)
                         {buildTableApply(cbind(b,a),tableClass="Class-table",skipHeader = TRUE)})

buildTableApply(class.stat.df[])

power.raw.df <- read.csv(power.raw, sep=";")%>% tbl_df()  #%>% arrange(Level,Category, Keywords, Name) %>% filter(Name!="")
power.raw.df <- gsubColwise(power.raw.df,"\\n","<br>")
usageOrder  <- c("","At-Will","Encounter","Daily")
power.raw.df$UsageLimit  <- factor(power.raw.df$UsageLimit, 
                                   levels=c(usageOrder,setdiff(power.raw.df$UsageLimit,usageOrder)))
power.raw.df  <- power.raw.df %>%  arrange(Class, isFeature!="Feature", Type, UsageLimit, Level, Name)

usageColors <- revalue(power.raw.df$UsageLimit,replace=c(Daily="Gray", Encounter="Red","AtWill"="Green"))
levels(usageColors)[levels(usageColors)==""] <- "Green"

power.raw.df.colored <- power.raw.df

power.raw.df$Name  <- paste("<div class=\"",usageColors,"\">",power.raw.df$Name, sep="")
power.raw.df$Level  <-paste(power.raw.df$Level,"</div>",sep="")

class.power.list  <- power.raw.df  %>%   split(power.raw.df$Class)


#power.tag.df <- read.csv(power.tag, sep=";")
power.tag.df <- read.csv(power.tag, sep=";", colClasses="character")
power.tag.pre<- power.tag.df[1,]
power.tag.post<- power.tag.df[2,]


css <- readChar(css.file, file.info(css.file)$size)


#Build power tables






power.table.htm<-buildTableApply(power.raw.df, 
                      df.names=c("Name", "Class", "Level", "Type","UsageLimit","Range","Action","Summary"),
                      tableClass="Power-table")





# power.table.htm<-paste(power.table.htm,collapse="<br> ")


write(power.table.htm,power.table.htm.file)


#Build full text descriptions

# comment this: power blocks not organized by class
# power.htm<-buildElementApply(power.raw.df %>%
#                                arrange(Class, isFeature!="Feature", Type, UsageLimit, Level, Name),
#                              power.tag.pre, power.tag.post, df.names=setdiff(names(power.raw.df),c("Summary")),skipEmpty = TRUE)


power.htm<-llply(class.power.list   ,
                      .fun=buildElementApply,
                 power.tag.pre, power.tag.post, df.names=setdiff(names(power.raw.df),c("Summary")),skipEmpty = TRUE)


power.htm  <- llply.name(power.htm,.fun=function(htm,name){paste("<p><h2>",name,"</h2></p><div class=\"Power-List\">",htm,"</div>" ,collapse="")})

power.htm <-paste(power.htm,collapse="<br> ")

power.full <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   power.table.htm,"<p></p>",
                   #"<div class=\"Power-List\">",
                   power.htm,
                   #"</div>",
                   class.stat.list[1],
                   "</body></html>",
                   sep="\r\n",
                   collapse="")


writeChar(power.full,power.htm.file)

