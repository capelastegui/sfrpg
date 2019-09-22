# library(dplyr)

#' Append tags before and after elements in a string array
#'
#' Typically used to convert raw data into html elements
#'
#' @param s string
#' @param pre string
#' @param post string
#' @param skipEmpty logical
#'
#' @return string
#' @export
#'
#'@examples
#' str_result = build_element(c('hello','world'),'<<', '>>')
#' 
build_element <- function(s, pre, post,skipEmpty=TRUE)  {
  if (skipEmpty){
    s  %>% purrr::map_chr(
      ~dplyr::if_else(is.na(.) | . == '',
               '', 
               paste0(pre, ., post)))
  } else {
    s  %>% purrr::map_chr(~paste0(pre, ., post))  }
  }


#' Convert dataframe columns into html elements, then reduce to single string
#'
#' @param df input dataframe
#' @param pre Dataframe with string columns and same names as df. Each column in
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
#' @exalmples
#'   df_pre = tibble::tibble(x='<x>', y='<y>' ,Body='(')
#'   df_post = tibble::tibble(x= '<x>', y='</y>', Body=')')
#'   build_element_apply(tibble::tibble(x=c('hello','world'),y=c('1', '2')), df_pre, df_post)
#'   
build_element_apply <- function (df,pre,post,
  df.names=names(df),skipEmpty=TRUE, collapse='') {

  pre <- pre %>% fillna_df()
  post <- post %>% fillna_df()

  df_tmp = df.names %>%
    purrr::map_dfc(~build_element(df[[.]], pre[[.]], post[[.]], skipEmpty))

  str_result = df_tmp %>%
    purrr::transpose() %>%
    purrr::map(paste0, collapse=collapse) %>%
    purrr::map_chr(~paste0(pre$Body, ., post$Body, collapse= "\r\n"))  %>%
    purrr::reduce(paste ,sep="\r\n", .init='')
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
#' build_table_apply(tibble::tibble(x=c('hello','world'),y=c('1', '2')) )
#' 
build_table_apply <- function (df, df.names=names(df),
  tableClass=NULL,skipHeader=FALSE)
{
  if (df %>% is.null()) {return("")}
  df = df %>% fillna_df()

  table_tag<-"<table>"
  if(!is.null(tableClass))
  {table_tag<-paste0("<table class=\"",tableClass,"\">")}
  
  if(!skipHeader){
    table_header<-purrr::map_chr(df.names, build_element, "<td>","</td>",skipEmpty=FALSE) %>%
      paste0(collapse='') %>% 
      (function(x) {paste0("<tr>",x,"</tr>", "\n",collapse="")})
  }else{table_header <- ""}
  
  table_body<-df[df.names] %>% purrr::map(build_element, "<td>","</td>",skipEmpty=FALSE) %>%
    dplyr::bind_cols() %>% 
    purrr::transpose() %>%
    purrr::map_chr(paste, collapse='') %>%
    (function(x) {paste0("<tr>",x,"</tr>", "\n",collapse="")}) %>% 
    (function (x) {paste0(table_header,x,collapse="\r\n")})

  result<-paste(table_tag,"\r\n<tbody>" ,table_body,"</tbody>\r\n</table>", "\n",
    collapse="")
  result
}


#' Apply gsub, then as.factor on string/factor columns of dataframe
#'
#' @param df Dataframe with string columns, used as input
#' @param pattern regex to apply
#' @param replacement replacement for pattern
#'
#' @return A dataframe like df1, with pattern replaced by replacement in columns
#' @export
#'
#' @examples
gsub_colwise <- function(df,pattern,replacement)
{
  replace_if_str <- function(x){
    if(is.character(x) | is.factor(x)) {
      stringr::str_replace(x, pattern, replacement)   
    } else {x}
  }  
  df %>% purrr::map_dfc(replace_if_str)
}

# TODO: Consider changing all factors to strings

#' Apply factor again on factor columns
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
refactor<-function(data)
  # Run factor() on each factor column
{
  data %>% purrr::map_if(is.factor, factor) %>% dplyr::bind_cols()
}


fillna_df <- function(df) {
  df %>% mutate_if (purrr::negate(is.factor),~tidyr::replace_na(.,''))
}
