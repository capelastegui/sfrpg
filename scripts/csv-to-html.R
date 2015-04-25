require(plyr)
require(dplyr)


buildElement <- function(str, pre, post,skipEmpty=FALSE)
{
  buildElementLoop  <- function(s,pre,post,skipEmpty){
    if(skipEmpty){
      emptyIndices <- is.na(s)||s==""
      pre <- as.character(pre)
      pre[emptyIndices] <- ""
      post  <- as.character(post)
      post[emptyIndices] <- ""
    }
    paste(pre,s,post,sep="")
  }
  #if(skipempty) pre and post become "" where s =""
  laply(str, pre,post, skipEmpty, .fun=buildElementLoop)
}

buildElementApply <- function (df,pre,post, df.names=names(df),skipEmpty=FALSE)
{
  tmp<-ldply(df.names,df,pre,post,.fun=function(n,df,pre,post){buildElement(df[[n]],pre[[n]],post[[n]],skipEmpty)})
  tmp<-laply(tmp,paste,sep="\r\n", collapse="\r\n")
  paste(pre$Body,tmp,post$Body, "\r\n",collapse="")
}

buildTableApply <- function (df, df.names=names(df), tableClass=NULL,skipHeader=FALSE)
{
  if(!skipHeader){
    tmp1<-paste0(laply(df.names, .fun=function(n){buildElement(n,"<td>","</td>")}),collapse="")
    tmp1<-paste0("<tr>",tmp1,"</tr>", "\n",collapse="")
  }else{tmp1 <- ""}
  
  tmp<-ldply(df.names,df,.fun=function(n,df){buildElement(df[[n]],"<td>","</td>")})
  tmp<-laply(tmp,paste0, collapse="")
  tmp<-paste0("<tr>",tmp,"</tr>", "\n",collapse="")
  tmp<-paste0(tmp1,tmp,collapse="\r\n")
  tableTag<-"<table>"
  if(!is.null(tableClass)) {tableTag<-paste0("<table class=\"",tableClass,"\">")}
  tmp<-paste(tableTag,"\r\n<tbody>" ,tmp,"</tbody>\r\n</table>", "\n",collapse="")
  tmp
}


#gsubColwise <-  colwise(.fun=(function(df1, pattern,replacement){gsub(pattern,replacement,df1)}))

gsubColwise <-  colwise(.fun=(function(df1, pattern,replacement)
  {
  tmp <-df1
  #if(is.character(df1)|is.factor(df1))   {tmp<- gsub(pattern,replacement,df1)}
  if(is.character(df1)|is.factor(df1))   {tmp<- as.factor(gsub(pattern,replacement,df1))}
  #if(is.factor(df1))  {tmp<-as.factor(gsub(pattern,replacement,df1))}
  tmp  
  }))

gsub.dataframe <- function(dataframe,pattern,replacement)
{

  df2<-dataframe
  gsubColwise <-  colwise(.fun=(function(df1, pattern,replacement){gsub(pattern,replacement,df1)}))

  
  factorIndices<-laply(df,is.factor)
  df2[,factorIndices]<-colwise(as.factor) (gsubColwise(df2[,factorIndices],pattern, replacement))


  charIndices<-laply(df,is.character)
  df2[,charIndices]<-gsubColwise(df2[,charIndices],pattern,replacement)
  #todo: maybe i have mixed up what should get cast to factor?
  df2
}


refactor<-function(data)
{colwise(function(col) {if (is.factor(col)) factor(col)  else col }) (data)
}
