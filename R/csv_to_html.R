#' Read and clean csv files
#'
#' @param s string , name of .csv file in /inst/raw/character_creation,
#         without extension
#' @param delim string, single character used to separate csv fields
#'
#' @return Dataframe generated from csv
#' @export
#'
#' @examples
#' df_origin_build <- read_my_csv('origin_build')
read_my_csv <- function(s, delim = ',', folder='character_creation') {
  dir_base  = system.file('raw', folder, package='sfrpg', mustWork=TRUE)

  str_regex = "\r[^\n]" # new lines with \r but not \r\n - to replace w \r\n
  readr::read_delim(
    file.path(dir_base, paste0(s, ".csv")),
    delim = delim,
    col_types = readr::cols(.default = "c")
  )  %>% dplyr::mutate_if (is.character,
                           ~stringr::str_replace_all(., str_regex, "\r\n"))
}


#' Append tags before and after elements in a string array
#'
#' Typically used to convert raw data into html elements
#'
#' @param s string array used as input
#' @param pre string with tags to append before s
#' @param post string with tags to append after s
#' @param skipEmpty logical
#'
#' @return string array with added tags
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
#' @examples
#'   df_pre = tibble::tibble(x='<x>', y='<y>' ,Body='(')
#'   df_post = tibble::tibble(x= '<x>', y='</y>', Body=')')
#'   build_element_apply(tibble::tibble(x=c('hello','world'),y=c('1', '2')), df_pre, df_post)
#'   
build_element_apply <- function (df,pre,post,
  df.names=names(df),skipEmpty=TRUE, collapse='', reduce=TRUE) {

  pre <- pre %>% fillna_df()
  post <- post %>% fillna_df()

  if (!'Body' %in% (pre %>% colnames())) {
    pre <- pre %>% dplyr::mutate(Body='')
    post <- post %>% dplyr::mutate(Body='')
  }

  df_tmp = df.names %>%
    purrr::map_dfc(~build_element(df[[.]], pre[[.]], post[[.]], skipEmpty))

  if (!reduce){
    colnames(df_tmp) <- df.names
    return(df_tmp)
  }

  str_result = df_tmp %>%
    purrr::transpose() %>%
    purrr::map(paste0, collapse=collapse) %>%
    purrr::map_chr(~paste0(pre$Body, ., post$Body, collapse= "\r\n"))  %>%
    purrr::reduce(paste ,
    sep="\r\n",
    .init='')
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
      (function(x) {paste0("<tr>",x,"</tr>", "\r\n",collapse="")})
  }else{table_header <- ""}
  
  table_body<-df[df.names] %>% purrr::map(build_element, "<td>","</td>",skipEmpty=FALSE) %>%
    dplyr::bind_cols() %>% 
    purrr::transpose() %>%
    purrr::map_chr(paste, collapse='') %>%
    (function(x) {paste0("<tr>",x,"</tr>", "\r\n",collapse="")}) %>%
    (function (x) {paste0(table_header,x,collapse="\r\n")})

  result<-paste(table_tag,"\r\n<tbody>" ,table_body,"</tbody>\r\n</table>", "\r\n",
    collapse="")
  result
}


#' Add html headers, css data to html string to use in standalone document
#'
#' @param htm_str
#'
#' @return
#' @export
#'
#' @examples
get_htm_file <- function(str_htm, css=NULL) {

  if (css %>% is.null()) {
    file_css <- system.file("SFRPG.css", package='sfrpg',
     mustWork=TRUE)
     css <- readChar(file_css, file.info(file_css)$size)
  }


  paste0(
    "<!DOCTYPE html>\r\n",
    "<html>\r\n<head>",
    "\r\n<title>Test tables</title>\r\n<style type=\"text/css\">\r\n",
    css,
    "\r\n</style></head>\r\n<body>\r\n",
    str_htm,
    "\r\n</body></html>",
    collapse = ""
  )
}


#' Apply gsub on string/factor columns of dataframe
#'
#' @param df Dataframe with string columns, used as input
#' @param pattern regex to apply
#' @param replacement replacement for pattern
#'
#' @return A dataframe like df1, with pattern replaced by replacement in columns
#' @export
#'
#' @examples
#' tibble::tibble(x=c('hello','world'),y=c('a', 'o'))  %>% gsub_colwise('o','u')
gsub_colwise <- function(df,pattern,replacement)
{
  replace_if_str <- function(x){
    if(is.character(x) | is.factor(x)) {
      stringr::str_replace_all(x, pattern, replacement)
    } else {x}
  }  
  df %>% purrr::map_dfc(replace_if_str)
}


# TODO: Remove references to this function, then delete. Factors no lnoger welcome.

#' Apply factor again on factor columns
#'
#' @param df df with factor columns
#'
#' @return df with refactorized columns
#' @export
#'
#' @examples
# refactor(iris)
refactor<-function(df)
  # Run factor() on each factor column
{
  df %>% purrr::map_if(is.factor, factor) %>% dplyr::tbl_df()
}

#' Fill NAs in non-factor columns with ''
#'
#' @param df input dataframe
#'
#' @return df with NA values replaced with ''
#' @export
#'
#' @examples
# fillna_df(tibble::tibble(x=c('hello',NULL),y=c(NULL, 'o')))
fillna_df <- function(df) {
  df %>% dplyr::mutate_if (purrr::negate(is.factor),~tidyr::replace_na(.,''))
}


#' Transpose dataframe, into (key, value) form
#'
#' @param df_class_stat
#'
#' @return
#' @export
#'
#' @examples
trans_df <- function(df_class_stat) {
  l_keys = df_class_stat %>% names()

  trans_df_apply <- function(colname, df_class_stat) {
    list(key = colname, value = df_class_stat[[colname]][[1]])
  }

  l_keys %>% purrr::map_dfr(trans_df_apply,
                            # Convert to string due to problem with factors
                            df_class_stat %>% purrr::map_dfc(as.character))

}