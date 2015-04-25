basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"

source(file.path(basedir,"scripts","csv-to-html.R"))

power.raw <- file.path(basedir,"raw","Powers-raw.csv")
power.tag <- file.path(basedir,"raw","Powers-tags.csv")
power.lesser.raw <- file.path(basedir,"raw","Powers-Lesser-raw.csv")
power.lesser.tag <- file.path(basedir,"raw","Powers-Lesser-tags.csv")
css.file <- file.path(basedir,"raw","SFRPG.css")
power.htm.file <- file.path(basedir,"html","CharacterCreation","Powers.html")
power.table.htm.file <- file.path(basedir,"html","CharacterCreation","Powers-table.html")

class.stat.raw <- file.path(basedir,"raw","Class-stats.csv")
class.stat.htm.file  <- file.path(basedir,"html","CharacterCreation","Class-stats.html")

class.features.raw <- file.path(basedir,"raw","Class-features.csv")
class.features.tag <- file.path(basedir,"raw","Class-features-tags.csv")



class.stat.df  <- read.csv(class.stat.raw, sep=";", header=TRUE)
class.feature.df  <- read.csv(class.features.raw, sep=";", header=TRUE)

class.stat.list  <- split(class.stat.df,class.stat.df$Class)
class.stat.list <- llply(class.stat.list, .fun=function(a){a <- refactor(a);split(a,a$Build)})
class.stat.list <- llply.n(class.stat.list,2,
                       .fun2=function(df){
                         table <- cbind(gsub("\\."," ",names(df)),trans_df(df))
                         htm <- buildTableApply(table,tableClass="Class-table", skipHeader = TRUE)
                         list(stats=table,stats.htm=htm)
                              })

usageOrder  <- c("","At-Will","Encounter","Daily")
power.tag.df <- read.csv(power.tag, sep=";", colClasses="character")
power.tag.pre<- power.tag.df[1,]
power.tag.post<- power.tag.df[2,]

feature.tag.df <- read.csv(class.features.tag, sep=";", colClasses="character")
feature.tag.pre<- feature.tag.df[1,]
feature.tag.post<- feature.tag.df[2,]

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
class.power.list <- llply.n(class.power.list,2,power.tag.pre, power.tag.post,
                           .fun2=function(df,power.tag.pre, power.tag.post){
                             htm <- buildElementApply(df,power.tag.pre, power.tag.post,
                                                      df.names=setdiff(names(df),c("Summary","Build","usageColors")),
                                                      skipEmpty = TRUE)
                             htm <- paste("<div class=\"Power-List\">",htm,"</div>" ,sep="")

                             table <- buildTableApply(df,
                                                    df.names=c("Name", "Class", "Level", "Type","UsageLimit","Range","Action","Summary"),
                                                    tableClass="Power-table")
                             list(powers=df,powers.htm=htm,powers.table=table)
                           })


class.power.list  <- llply.n(class.power.list,2,
                             .fun=function(l){
                               l$classbuild  <- paste("<h2>",l$powers$Class[1],l$powers$Build[1],"</h2>",sep=" ")
                               l})


class.feature.list  <- split(class.feature.df,class.feature.df$Class) 
class.feature.list <- llply(class.feature.list, .fun=function(a){a <- refactor(a);split(a,a$Build)})
class.feature.list <- llply.n(class.feature.list,2,feature.tag.pre, feature.tag.post,
                            .fun2=function(df,feature.tag.pre, feature.tag.post){
                              htm <- buildElementApply(df,feature.tag.pre, feature.tag.post,
                                                       df.names=setdiff(names(df),c("Class","Build")),
                                                       skipEmpty = TRUE)
                              #htm <- paste("<div class=\"Power-List\">",htm,"</div>" ,sep="")
                              
                              list(features=df,features.htm=htm)
                            })



#join all class nested lists
class.list  <- llply.parallel.multilist(class.power.list, 
                                        list(class.power.list,class.stat.list,class.feature.list),
                                        n=2,
                                        .fun=function(...){unlist(c(...),recursive=FALSE)})




css <- readChar(css.file, file.info(css.file)$size)

#Build power tables
#add stuff to nested list
#class.list  <- llply.n(class.list,2,.fun2 = function(...){c(...,a="A")})
# No longer used, keep in case we need a global power table
# power.table.htm<-buildTableApply(power.raw.df, 
#                       df.names=c("Name", "Class", "Level", "Type","UsageLimit","Range","Action","Summary"),
#                       tableClass="Power-table")
# power.table.htm<-paste(power.table.htm,collapse="<br> ")
#write(power.table.htm,power.table.htm.file)

#Build full text descriptions
power.full <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   "<p>",class.list$Fighter$Guardian$stats.htm,"</p>",
                   "<p>",class.list$Fighter$Guardian$powers.table,"</p>",
                   class.list$Fighter$Guardian$powers.htm,
                   "</body></html>",
                   sep="\r\n",
                   collapse="")


tmp <- llply.n(class.list,2,
               .fun=function(l){
                 paste("<p>",l$classbuild,"</p>\r\n",
                       "<p><h3>Class Stats</h3></p>\r\n",
                       "<p>",l$stats.htm,"</p>\r\n",
                       "<p><h3>Class features</h3></p>\r\n",
                       "<p>",l$features.htm,"</p>\r\n",
                       "<p><h3>Class Powers</h3></p>\r\n",
                       #"<p>",l$powers.table,"</p>\r\n",
                       "<p>",l$powers.htm,"</p>\r\n",
                       sep="")
               })


tmp <- paste(llply(tmp,collapse="",.fun=paste),collapse="\r\n")

power.full <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                    css,
                    "</style></head>\r\n<body>",
                    tmp,
                    "</body></html>",
                    sep="",
                    collapse="")


# power.full  <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
#                      css,
#                      "</style></head>\r\n<body>",


writeChar(power.full,power.htm.file)

