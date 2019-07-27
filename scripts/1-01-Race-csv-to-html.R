
getRaceList <- function (basedir=here::here())
{
  require(rutils)
  source(file.path(basedir,"scripts","0-00-csv-to-html.R"))

#File paths
## Race powers
race.power.raw <- file.path(basedir,"raw","charactercreation","Race-Powers-raw.csv")
race.power.tag <- file.path(basedir,"raw","charactercreation","Race-Powers-tags.csv")
css.file <- file.path(basedir,"Rmd","SFRPG.css")
## Race stats
race.stat.raw <- file.path(basedir,"raw","charactercreation","Race-stats.csv")
race.stat.htm.file  <- file.path(basedir,"html","CharacterCreation","Race-stats.html")
## Race features
race.features.raw <- file.path(basedir,"raw","charactercreation","Race-features.csv")
race.features.tag <- file.path(basedir,"raw","charactercreation","Race-features-tags.csv")
## Race html
race.power.htm.file <- file.path(basedir,"html","CharacterCreation","race-Powers.html")
race.power.table.htm.file <- file.path(basedir,"html","CharacterCreation","race-Powers-table.html")

#Data frames
race.stat.df  <- read.csv(race.stat.raw, sep=";", header=TRUE)
race.feature.df  <- read.csv(race.features.raw, sep=";", header=TRUE) %>%  gsubColwise("\\n","<br>")
#Lists
race.stat.list  <- split(race.stat.df,race.stat.df$Race)
race.stat.list <- llply(race.stat.list, .fun=function(a){a <- refactor(a);split(a,a$Subrace)})
race.stat.list <- llply.n(race.stat.list,2,
                       .fun2=function(df){
                         table <- cbind(gsub("\\."," ",names(df)),trans_df(df))
                         htm <- buildTableApply(table,tableClass = "Race-table", skipHeader = TRUE)
                         list(stats=table,stats.htm=htm)
                              })

usageOrder  <- c("","At-Will","Encounter","Daily")
race.power.tag.df <- read.csv(race.power.tag, sep=";", colClasses = "character")
race.power.tag.pre<- race.power.tag.df[1,]
race.power.tag.post<- race.power.tag.df[2,]

feature.tag.df <- read.csv(race.features.tag, sep=";", colClasses="character")
feature.tag.pre<- feature.tag.df[1,]
feature.tag.post<- feature.tag.df[2,]

race.power.raw.df <- read.csv(race.power.raw, sep=";") %>% 
  gsubColwise("\\n","<br>")%>% 
  tbl_df() %>% 
  mutate(UsageLimit=factor(UsageLimit, 
                           levels=c(usageOrder,setdiff(UsageLimit,usageOrder)))) %>%  
  arrange(Race, isFeature!="Feature", Type, UsageLimit, Level, Name) %>% 
  mutate(usageColors=revalue(UsageLimit,replace=c(Daily="gray", Encounter="red","At-Will"="green")))

levels(race.power.raw.df$usageColors)[levels(race.power.raw.df$usageColors)==""] <- "green"

race.power.raw.df  <- race.power.raw.df %>% 
  mutate(Name=paste("<span class=\"",usageColors,"\">",Name, sep="")) %>%
  mutate(Level=paste(Level,"</span>",sep=""))


race.power.list  <- split(race.power.raw.df,race.power.raw.df$Race) 
race.power.list <- llply(race.power.list, .fun=function(a){a <- refactor(a);split(a,a$Subrace)})
race.power.list <- llply.n(race.power.list,2,race.power.tag.pre, race.power.tag.post,
                           .fun2=function(df,race.power.tag.pre, race.power.tag.post){
                             htm <- buildElementApply(df,race.power.tag.pre, race.power.tag.post,
                                                      df.names=setdiff(names(df),c("Summary","Subrace","usageColors")),
                                                      skipEmpty = TRUE)
                             htm <- paste("<div race=\"Power-List\">",htm,"</div>" ,sep="")

                             table <- buildTableApply(df,
                                                    df.names=c("Name", "Race", "Level", "Type","UsageLimit","Range","Action","Summary"),
                                                    tableClass = "Power-table")
                             list(powers=df,powers.htm=htm,powers.table=table)
                           })


race.power.list  <- llply.n(race.power.list,2,
                             .fun=function(l){
                               l$racebuild  <- paste("<h2>",l$powers$Subrace[1],"</h2>",sep=" ")
                               l})


race.feature.list  <- split(race.feature.df,race.feature.df$Race) 
race.feature.list <- llply(race.feature.list, .fun=function(a){a <- refactor(a);split(a,a$Subrace)})
race.feature.list <- llply.n(race.feature.list,2,feature.tag.pre, feature.tag.post,
                            .fun2=function(df,feature.tag.pre, feature.tag.post){
                              htm <- buildElementApply(df,feature.tag.pre, feature.tag.post,
                                                       df.names=setdiff(names(df),c("Race","Subrace")),
                                                       skipEmpty = TRUE)
                              #htm <- paste("<div race=\"Power-List\">",htm,"</div>" ,sep="")
                              
                              list(features=df,features.htm=htm)
                            })



#join all race nested lists
race.list  <- llply.parallel.multilist(race.stat.list, 
                                        list(race.power.list,race.stat.list,race.feature.list),
                                        n=2,
                                        .fun=function(...){unlist(c(...),recursive=FALSE)})



race.list
}

writeraceList  <- function(race.list)
{
  
  css.file <- file.path(basedir,"Rmd","SFRPG.css")


race.stat.htm.file  <- file.path(basedir,"html","CharacterCreation","race-stats.html")



race.power.htm.file <- file.path(basedir,"html","CharacterCreation","Powers.html")
race.power.table.htm.file <- file.path(basedir,"html","CharacterCreation","Powers-table.html")

#Build power tables
#add stuff to nested list
#race.list  <- llply.n(race.list,2,.fun2 = function(...){c(...,a="A")})
# No longer used, keep in case we need a global power table
# race.power.table.htm<-buildTableApply(race.power.raw.df, 
#                       df.names=c("Name", "race", "Level", "Type","UsageLimit","Range","Action","Summary"),
#                       tablerace="Power-table")
# race.power.table.htm<-paste(race.power.table.htm,collapse="<br> ")
#write(race.power.table.htm,race.power.table.htm.file)

#Build full text descriptions
race.power.full <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   "<p>",race.list$Human$Human$stats.htm,"</p>",
                   "<p>",race.list$Human$Human$powers.table,"</p>",
                   race.list$Human$Human$powers.htm,
                   "</body></html>",
                   sep="\r\n",
                   collapse="")


tmp <- llply.n(race.list,2,
               .fun=function(l){
                 paste("<p>",l$racebuild,"</p>\r\n",
                       "<p><h3>race Stats</h3></p>\r\n",
                       "<p>",l$stats.htm,"</p>\r\n",
                       "<p><h3>race features</h3></p>\r\n",
                       "<p>",l$features.htm,"</p>\r\n",
                       "<p><h3>race Powers</h3></p>\r\n",
                       #"<p>",l$powers.table,"</p>\r\n",
                       "<p>",l$powers.htm,"</p>\r\n",
                       sep="")
               })


tmp <- paste(llply(tmp,collapse="",.fun=paste),collapse="\r\n")

race.power.full <- paste("<html>\r\n<head>\r\n<title>class-power-test</title>\r\n<style type=\"text/css\">",
                    css,
                    "</style></head>\r\n<body>",
                    tmp,
                    "</body></html>",
                    sep="",
                    collapse="")




writeChar(race.power.full,race.power.htm.file)
raceList
 }
