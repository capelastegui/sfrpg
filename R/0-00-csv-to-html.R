require(plyr)
require(dplyr)

buildElement <- function(str, pre, post,skipEmpty=TRUE)  {
  # On a dataframe column of strings,
  # appends strings before and after each element
  #
  # Args:
  #   str: String vector taken as input
  #   pre: String to append before input
  #   post: String to append after input
  #   skipEmpty: If True, null values of str will be converted to ''.
  # Returns:
  #   A string array pasting pre, str and post.
  #
  # Used to convert dataframes into html tables
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
  # if(skipempty) pre and post become "" where s =""
  laply(str, pre, post, skipEmpty, .fun=buildElementLoop)
}


buildElementApply <- function (df,pre,post,
  df.names=names(df),skipEmpty=TRUE)
  # On an input dataframe of string columns, run BuildElement() on
  # each column, then paste each row into a single string, then
  # paste all rows into a single string and append strings before and after.
  #
  # Args:
  #   df: Dataframe with string columns, used as input
  #   pre: Dataframe with string columns and same names as df. Each column in
  #        pre contains a string value to append before the corresponding
  #        column in df. Includes a Body column, to be appended before the
  #        aggregated output.
  #   post: Dataframe with string columns and same names as df. Each column in
  #        post contains a string value to append before the corresponding
  #        column in df. Includes a Body column, to be appended after the
  #        aggregated output.
  #   skipEmpty: If True, null values of str will be converted to ''.
  # Returns:
  #   A string array pasting pre, str and post.
  #
  # Used to convert dataframes into html elements
{
  tmp<-ldply(df.names,df,pre,post,
    .fun=function(n,df,pre,post)
      {buildElement(df[[n]],pre[[n]],post[[n]],skipEmpty)})
  tmp<-laply(tmp,paste,sep="", collapse="")
  paste(pre$Body,tmp,post$Body, "\r\n",collapse="")
}


buildTableApply <- function (df, df.names=names(df),
  tableClass=NULL,skipHeader=FALSE)
{
  # Convert an input dataframe of string columns into an HTML table
  #
  # Args:
  #   df: Dataframe with string columns, used as input
  #   df.names: List of dataframe columns to include in output.
  #   tableClass: value of class html attribute of table. Use to identify
  #               the table, apply CSS format.
  #   skipHeader: If FALSE, add df.names as column names in output table.
  # Returns:
  #   A string array with an HTML table
  #
  # Used to convert dataframes into html elements
  if(!skipHeader){
    tmp1<-paste0(laply(df.names,
      .fun=function(n){buildElement(n,"<td>","</td>",skipEmpty=FALSE)}),
      collapse="")
    tmp1<-paste0("<tr>",tmp1,"</tr>", "\n",collapse="")
  }else{tmp1 <- ""}
  
  tmp<-ldply(df.names,df,
    .fun=function(n,df){buildElement(df[[n]],"<td>","</td>",skipEmpty=FALSE)})
  tmp<-laply(tmp,paste0, collapse="")
  tmp<-paste0("<tr>",tmp,"</tr>", "\n",collapse="")
  tmp<-paste0(tmp1,tmp,collapse="\r\n")
  tableTag<-"<table>"
  if(!is.null(tableClass))
    {tableTag<-paste0("<table class=\"",tableClass,"\">")}
  tmp<-paste(tableTag,"\r\n<tbody>" ,tmp,"</tbody>\r\n</table>", "\n",
    collapse="")
  tmp
}


gsubColwise <-  colwise(.fun=(function(df1, pattern,replacement)
  # Function applied colwise to a dataframe,
  # running gsub on string or factor columns, and then converting
  # any string columns to factors
  #
  # Args:
  #   df1: Dataframe with string columns, used as input
  #   pattern: regex to apply
  #   replacement: replacement for pattern
  # Returns:
  #   A dataframe like df1, with pattern replaced by replacement in columns
  {
  tmp <-df1
  if(is.character(df1)|is.factor(df1))
    {tmp<- as.factor(gsub(pattern,replacement,df1))}
  tmp
  }))


gsub.dataframe <- function(dataframe,pattern,replacement)
  # Function applied colwise to a dataframe,
  # running gsub on string or factor columns
  #
  # Args:
  #   df1: Dataframe with string columns, used as input
  #   pattern: regex to apply
  #   replacement: replacement for pattern
  # Returns:
  #   A dataframe like df1, with pattern replaced by replacement in columns
{

  df2<-dataframe
  gsubColwise <-  colwise(.fun=(function(df1, pattern,replacement)
    {gsub(pattern,replacement,df1)}))

  factorIndices<-laply(df,is.factor)
  # Run gsubColwise on df2, then run colwise(as.factor) on output
  df2[,factorIndices]<-colwise(as.factor)
    (gsubColwise(df2[,factorIndices],pattern, replacement))

  charIndices<-laply(df,is.character)
  df2[,charIndices]<-gsubColwise(df2[,charIndices],pattern,replacement)
  #todo: maybe i have mixed up what should get cast to factor?
  df2
}


refactor<-function(data)
  # Run factor() on each factor column
{
  colwise(function(col)
    {if (is.factor(col)) factor(col)  else col }) (data)
}
