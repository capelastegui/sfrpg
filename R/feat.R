#' convert feat table to html
#'
#' @param df_feat
#'
#' @return
#' @export
get_htm_feat <- function(df_feat=NULL){

  if (df_feat %>% is.null()) {
    df_feat <- read_my_csv('feats')
  }
  df_feat <- df_feat %>% gsub_colwise("\\r\\n", "<br>")
  df_tag <-  read_my_csv('feats_tags')
  tag_pre <- df_tag[1,]
  tag_post <- df_tag[2,]

  htm_feat_raw <- build_element_apply(df_feat, tag_pre, tag_post,
                      df.names=setdiff(names(df_feat), c('Summary')))

  htm_feat <- htm_feat_raw %>%
    # [H] is converted to link in markdown - need to escape
    # , also \\[H\\] is converted into something else
    #stringr::str_replace_all("\\r\\n", "<br>") %>%
    stringr::str_replace_all('\\[', '&#91;') %>%
    stringr::str_replace_all('\\]', '&#93;')

  htm_feat
}

#' Add feat list div to feat html
#'
#' @param htm_feat
#'
#' @return
#' @export
get_feat_list_htm <- function(htm_feat){
    paste0('<div class="Feat-List">',htm_feat,'</div>')
}

#' add feat table div to feat html
#'
#' @param htm_feat_table
#'
#' @return
#' @export
get_feat_table_htm <- function(htm_feat_table){
    paste0('<div class="Feat-Table">',htm_feat_table,'</div>')
}



#' Write a feat table as an html file
#'
#' @param htm_feat
#' @param dir_output
#' @param file_name
#'
#' @return
#' @export
write_file_htm_feat <- function(htm_feat, dir_output, file_name){
    file_css <- system.file("SFRPG.css", package='sfrpg',
        mustWork=TRUE)
    css <- readChar(file_css, file.info(file_css)$size)

    htm_full <- paste("<!DOCTYPE html>",
        "<html>\r\n<head>\r\n<title>Feats</title>",
        "\r\n<style type=\"text/css\">",
        css,
        "</style></head>\r\n<body>",
        htm_feat %>% get_feat_list_htm(),
        "</body></html>",
        sep="\r\n",
        collapse="<br>")
    writeChar(htm_full, file.path(dir_output, file_name))
}

# TODO: Obsolete, update
getFeatList <- function (dir_base=here::here())
{
  source(file.path(dir_base,"R","0-00-csv-to-html.R"))
  
  feat.raw <- file.path(dir_base,"inst", "raw","charactercreation","Feats-raw.csv")
  feat.tag <- file.path(dir_base,"inst", "raw","charactercreation","Feats-tags.csv")
  feat.lesser.raw <- file.path(dir_base,"inst", "raw","charactercreation","Feats-Lesser-raw.csv")
  feat.lesser.tag <- file.path(dir_base,"inst", "raw","charactercreation","Feats-Lesser-tags.csv")
  css.file <- file.path(dir_base,"Rmd","SFRPG.css")
  feat.htm.file <- file.path(dir_base,"html","CharacterCreation","Feats.html")
  feat.table.htm.file <- file.path(dir_base,"html","CharacterCreation","Feats-table.html")
  feat.lesser.htm.file <- file.path(dir_base,"html","CharacterCreation","Feats-lesser.html")
  feat.lesser.table.htm.file <- file.path(dir_base,"html","CharacterCreation","Feats-lesser-table.html")
  
  feat.raw.df <- read.csv(feat.raw, sep=";")%>% tbl_df()  %>% arrange(Level,Category, Keywords, Name) %>%
    dplyr::filter(Name!="")
  feat.raw.df <- gsub_colwise(feat.raw.df,"\\n","<br>")
  
  feat.lesser.raw.df <- read.csv(feat.lesser.raw, sep=";")%>% tbl_df()  %>% arrange(Level,Keywords, Name) %>%
    dplyr::filter(Name!="")
  feat.lesser.raw.df <- gsub_colwise(feat.lesser.raw.df,"\\n","<br>")
  
  feat.tag.df <- read.csv(feat.tag, sep=";")
  feat.tag.pre<- feat.tag.df[1,]
  feat.tag.post<- feat.tag.df[2,]
  
  feat.lesser.tag.df <- read.csv(feat.lesser.tag, sep=";")
  feat.lesser.tag.pre<- feat.lesser.tag.df[1,]
  feat.lesser.tag.post<- feat.lesser.tag.df[2,]
  
  
  
  
  css <- readChar(css.file, file.info(css.file)$size)
  
  #Build feat tables
  
  
  feat.list.raw <- llply(split(feat.raw.df,feat.raw.df$Level),.fun=applysplit, splitvar="Category")
  feat.list.table  <- llply.2(feat.list.raw,.fun=build_table_apply,
                              df.names=setdiff(names(feat.raw.df),"Text"))
  feat.list.htm  <- llply.2(feat.list.raw,.fun=build_element_apply,
                            feat.tag.pre, feat.tag.post, df.names=setdiff(names(feat.raw.df),"Summary"))
  
  
  feat.lesser.list.raw  <- llply(split(feat.lesser.raw.df,feat.lesser.raw.df$Level),.fun=applysplit, splitvar="Keywords")
  feat.lesser.list.table  <- llply.2(feat.lesser.list.raw,.fun=build_table_apply,
                                     df.names=setdiff(names(feat.lesser.raw.df),"Text"))
  feat.lesser.list.htm  <- llply.2(feat.lesser.list.raw,.fun=build_element_apply,
                                   feat.lesser.tag.pre, feat.lesser.tag.post, df.names=setdiff(names(feat.lesser.raw.df),"Summary"))
  
  
  feat.list <- list()
  
  feat.list$feats <- 
    mapply(FUN=function(a,b,c)
    {
      mapply(a,b,c,SIMPLIFY=FALSE, FUN=function(a,b,c){list(table=a,htm=b,raw=c)})
    },
    feat.list.table,feat.list.htm,feat.list.raw, SIMPLIFY=FALSE)
  
  feat.list$feats  <- getFullFeatHtm(feat.list$feats)
  
  feat.list$lesserfeats <- 
    mapply(FUN=function(a,b)
    {
      mapply(a,b,SIMPLIFY=FALSE, FUN=function(a,b){list(table=a,htm=b)})
    },
    feat.lesser.list.table,feat.lesser.list.htm, SIMPLIFY=FALSE)
  
  feat.list$lesserfeats  <- getFullFeatHtm(feat.list$lesserfeats)
  
  feat.list$tag$pre <-feat.tag.pre
  feat.list$tag$post <-feat.tag.post
  
  feat.list
}






