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



class.stat.df  <- read.csv(class.stat.raw, sep=";", header=TRUE)

class.stat.list  <- split(class.stat.df,class.stat.df$Class)
class.stat.list <- llply(class.stat.list, .fun=function(a){a <- refactor(a);split(a,a$Build)})
class.stat.list <- llply.n(class.stat.list,2,
                       .fun2=function(df){
                         table <- cbind(names(df),trans_df(df))
                         htm <- buildTableApply(table,tableClass="Class-table")
                         list(stats=table,stats.htm=htm)
                              })

usageOrder  <- c("","At-Will","Encounter","Daily")

power.raw.df <- read.csv(power.raw, sep=";") %>% 
  gsubColwise("\\n","<br>")%>% 
  tbl_df() %>% 
  mutate(UsageLimit=factor(UsageLimit, 
                           levels=c(usageOrder,setdiff(UsageLimit,usageOrder)))) %>%  
  arrange(Class, isFeature!="Feature", Type, UsageLimit, Level, Name) %>% 
  mutate(usageColors=revalue(UsageLimit,replace=c(Daily="Gray", Encounter="Red","At-Will"="Green")))

levels(power.raw.df$usageColors)[levels(power.raw.df$usageColors)==""] <- "Green"

power.raw.df  <- power.raw.df %>% 
  mutate(Name=paste("<div class=\"",usageColors,"\">",Name, sep="")) %>%
  mutate(Level=paste(Level,"</div>",sep=""))


class.power.list  <- split(power.raw.df,power.raw.df$Class) 
class.power.list <- llply(class.power.list, .fun=function(a){a <- refactor(a);split(a,a$Build)})
class.power.list <- llply.n(class.power.list,2,
                           .fun2=function(df){
                             htm <- buildTableApply(table,tableClass="Class-table")
                             list(powers=df,powers.htm=htm)
                           })

#join all class nested lists
class.list  <- llply.parallel.multilist(class.power.list, 
                                        list(class.power.list,class.stat.list),
                                        n=2,
                                        .fun=function(...){unlist(c(...),recursive=FALSE)})

llply.parallel.multilist(mylist1,list(mylist1,mylist2),1,.fun=function(...){unlist(c(...),recursive=FALSE)})

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
                 power.tag.pre, power.tag.post, df.names=setdiff(names(power.raw.df),c("Summary","Build")),skipEmpty = TRUE)


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

