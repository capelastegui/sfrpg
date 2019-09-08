
get_l_class <- function (dir_base=here::here())
{
  #require(rutils)
  source(file.path(dir_base,"R","0-00-csv-to-html.R"))
  source(file.path(dir_base,"R","utils.R"))

  read_my_csv <- function(s) {
  readr::read_delim(file.path(dir_base,"raw","CharacterCreation",paste0(s,".csv")),
                    delim=";", col_types = readr::cols(.default = "c"))}

    #class.stat.htm.file  <- file.path(dir_base,"html","CharacterCreation","Class-stats.html")
    #power.table.htm.file <- file.path(dir_base,"html","CharacterCreation","Powers-table.html")

    usageOrder  <- c("","At-Will","Encounter","Daily")

    df_class_stat = read_my_csv('Class-stats') # TODO: header=TRUE, if required
    df_class_feature = read_my_csv('Class-features') %>%  gsub_colwise("\\n","<br>") # TODO: header=TRUE, if required
    # Todo: convert columns to character if required
    df_feature_tag <- read_my_csv('Class-features-tags')
    df_power_tag <-  read_my_csv('Powers-tags')
    df_power_raw <-  read_my_csv('Powers-raw') %>%
      gsub_colwise("\\n","<br>")%>%
      fillna_df %>%
      mutate(UsageLimit=factor(UsageLimit,
                               levels=c(usageOrder,setdiff(UsageLimit,usageOrder)))) %>%
      arrange(Class, isFeature!="Feature", Type, UsageLimit, Level, Name) %>%
      mutate(usageColors=revalue(UsageLimit,replace=c(Daily="gray", Encounter="red","At-Will"="green")))

    levels(df_power_raw$usageColors)[levels(df_power_raw$usageColors)==""] <- "green"

    df_power_raw  <- df_power_raw %>%
      mutate(Name=paste("<span class=\"",usageColors,"\">",Name, sep="")) %>%
      mutate(Level=paste(Level,"</span>",sep=""))

    l_class_stat  <- split(df_class_stat, df_class_stat$Class)
    l_class_stat <- llply(l_class_stat, .fun=function(a){a <- refactor(a);split(a,a$Build)})
    l_class_stat <- llply.n(l_class_stat,2,
                           .fun2=function(df){
                             table <- cbind(gsub("\\."," ",names(df)),trans_df(df))
                             htm <- build_table_apply(table,tableClass="Class-table", skipHeader = TRUE)
                             list(stats=table,stats.htm=htm)
                                  })

    power_tag_pre<- df_power_tag[1,]
    power_tag_post<- df_power_tag[2,]

    feature_tag_pre<- df_feature_tag[1,]
    feature_tag_post<- df_feature_tag[2,]

    l_class_power  <- split(df_power_raw,df_power_raw$Class)
    l_class_power <- llply(l_class_power, .fun=function(a){a <- refactor(a);split(a,a$Build)})
    l_class_power <- llply.n(l_class_power,2,power_tag_pre, power_tag_post,
                               .fun2=function(df,power.tag.pre, power.tag.post){
                                 htm <- build_element_apply(df,power.tag.pre, power.tag.post,
                                                          df.names=setdiff(names(df),c("Summary","Build","usageColors")),
                                                          skipEmpty = TRUE, collapse = ' ')
                                 htm <- paste("<div class=\"Power-List\">",htm,"</div>" ,sep="")

                                 table <- build_table_apply(df,
                                                        df.names=c("Name", "Class", "Level", "Type","UsageLimit","Range","Action","Summary"),
                                                        tableClass="Power-table")
                                 list(powers=df,powers.htm=htm,powers.table=table)
                               })
    l_class_power  <- llply.n(l_class_power,2,
                                 .fun=function(l){
                                   l$classbuild  <- paste("<h2>",l$powers$Class[1],l$powers$Build[1],"</h2>",sep=" ")
                                   l})


l_class_feature  <- split(df_class_feature,df_class_feature$Class)
l_class_feature <- llply(l_class_feature, .fun=function(a){a <- refactor(a);split(a,a$Build)})
l_class_feature <- llply.n(l_class_feature,2,feature_tag_pre, feature_tag_post,
                            .fun2=function(df,feature.tag.pre, feature.tag.post){
                              htm <- build_element_apply(df,feature.tag.pre, feature.tag.post,
                                                       df.names=setdiff(names(df),c("Class","Build")),
                                                       skipEmpty = TRUE)
                              #htm <- paste("<div class=\"Power-List\">",htm,"</div>" ,sep="")
                              
                              list(features=df,features.htm=htm)
                            })

#join all class nested lists
l_class  <- llply.parallel.multilist(l_class_power,
                                        list(l_class_power,l_class_stat,l_class_feature),
                                        n=2,
                                        .fun=function(...){unlist(c(...),recursive=FALSE)})
l_class
}

writeClassList  <- function(l_class)
{
  
  css.file <- file.path(dir_base,"Rmd","SFRPG.css")

class.stat.htm.file  <- file.path(dir_base,"html","CharacterCreation","Class-stats.html")

power.htm.file <- file.path(dir_base,"html","CharacterCreation","Powers.html")
power.table.htm.file <- file.path(dir_base,"html","CharacterCreation","Powers-table.html")

#Build power tables
#add stuff to nested list
#l_class  <- llply.n(l_class,2,.fun2 = function(...){c(...,a="A")})
# No longer used, keep in case we need a global power table
# power.table.htm<-build_table_apply(power.raw.df,
#                       df.names=c("Name", "Class", "Level", "Type","UsageLimit","Range","Action","Summary"),
#                       tableClass="Power-table")
# power.table.htm<-paste(power.table.htm,collapse="<br> ")
#write(power.table.htm,power.table.htm.file)

#Build full text descriptions
power.full <- paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                   css,
                   "</style></head>\r\n<body>",
                   "<p>",l_class$Fighter$Guardian$stats.htm,"</p>",
                   "<p>",l_class$Fighter$Guardian$powers.table,"</p>",
                   l_class$Fighter$Guardian$powers.htm,
                   "</body></html>",
                   sep="\r\n",
                   collapse="")

tmp <- llply.n(l_class,2,
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

writeChar(power.full,power.htm.file)
classList
 }
