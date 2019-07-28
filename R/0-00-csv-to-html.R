# library(plyr)
# library(dplyr)

#' Append tags before and after elements in a string array
#'
#' Typically used to convert raw data into html elements
#'
#' @param str string
#' @param pre string
#' @param post string
#' @param skipEmpty logical
#'
#' @return string
#' @export
#'
#'#' @examples
#' str_result = buildElement(c('hello','world'),'<<', '>>')
#' 
buildElement <- function(s, pre, post,skipEmpty=TRUE)  {
  if (skipEmpty){
    s  %>% purrr::map_chr(
      ~if_else(is.na(.) | . == '',
               '', 
               paste0(pre, ., post)))
  } else {
    s  %>% purrr::map_chr(~paste0(pre, ., post))  }
  }


#' Convert dataframe columns into html elements, then reduce to single string
#'
#' @param df input dataframe
#' @param preDataframe with string columns and same names as df. Each column in
#'        pre contains a string value to append before the corresponding
#'        column in df. Includes a Body column, to be appended before the
#'        aggregated output.
#' @param post Dataframe with string columns and same names as df. Each column in
#'        post contains a string value to append before the corresponding
#'        column in df. Includes a Body column, to be appended after the
#'        aggregated output.
#' @param df.names names of columns to use
#' @param skipEmpty If True, null values of str will be converted to ''.
#'
#' @return character
#' @export
#'
#' @examples
#'   df_pre = tibble(x='<x>', y='<y>' ,Body='(')
#'   df_post = tibble (x= '<x>', y='</y>', Body=')')
#'   buildElementApply(tibble(x=c('hello','world'),y=c('1', '2')), df_pre, df_post)
#'   
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
  df_tmp = df.names %>%
    map_dfc(~buildElement(df[[.]], pre[[.]], post[[.]], skipEmpty))

  str_result = df_tmp %>%
    transpose %>%
    map(paste0, collapse="") %>%
    map_chr(~paste0(pre$Body, ., post$Body, collapse= "\r\n"))  %>%
    reduce(paste ,sep="\r\n")
  str_result
}


#' Convert an input dataframe of string columns into an HTML table
#'
#' @param df  Dataframe with string columns, used as input.
#' @param df.names List of dataframe columns to include in output.
#' @param tableClass value of class html attribute of table. Use to identify
#'  the table, apply CSS format.
#' @param skipHeader If FALSE, add df.names as column names in output table.
#'   Otherwise, use first row as column names. 
#'
#' @return A string with an HTML table
#' @export
#'
#' @examples
#' buildTableApply(tibble(x=c('hello','world'),y=c('1', '2')) )
#' 
buildTableApply <- function (df, df.names=names(df),
  tableClass=NULL,skipHeader=FALSE)
{
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


#' Apply gsub, then as.factor on string/factor columns of dataframe
#'
#' @param dataframe Dataframe with string columns, used as input
#' @param pattern regex to apply
#' @param replacement replacement for pattern
#'
#' @return A dataframe like df1, with pattern replaced by replacement in columns
#' @export
#'
#' @examples
gsubColwise <- function(df,pattern,replacement)
{
  replace_if_str <- function(x){
    if(is.character(x) | is.factor(x)) {
      stringr::str_replace(x, pattern, replacement)   
    } else {x}
  }  
  df %>% purrr::map_dfc(replace_if_str)
}


#' Title
#'
#' @param dataframe Dataframe with string columns, used as input
#' @param pattern regex to apply
#' @param replacement replacement for pattern
#'
#' @return
#' @export
#'
#' @examples
gsub.dataframe <- function(dataframe,pattern,replacement)
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
