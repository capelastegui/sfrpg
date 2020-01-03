#' Title
#'
#' @param s 
#' @param delim 
#'
#' @return
#' @export
read_csv_monster <- function(s, delim=',') {
    read_my_csv(s, delim, 'monsters')
}

#' Title
#'
#' @param df_monster_race 
#'
#' @return
#' @export
get_htm_monster_race <- function(df_monster_race=NULL){

  if (df_monster_race %>% is.null()) {
    df_monster_race <- read_csv_monster("monster_races")
  }
  df_monster_race <- df_monster_race %>% dplyr::select(-category)
  df_tag <-  read_csv_monster("Monsters-race-tags",';')
  tag_pre <- df_tag[1,]
  tag_post <- df_tag[2,]

  build_element_apply(df_monster_race, tag_pre, tag_post,
                      df.names=names(df_monster_race))
}


#' Title
#'
#' @param df_monster_class 
#'
#' @return
#' @export
get_htm_monster_class <- function(df_monster_class=NULL){

      if (df_monster_class %>% is.null()) {
    df_monster_class <- read_csv_monster("monster_classes")
  }
  df_monster_class <- df_monster_class %>% dplyr::select(-category)
  df_tag <-  read_csv_monster("Monsters-class-tags",';')
  tag_pre <- df_tag[1,]
  tag_post <- df_tag[2,]

  build_element_apply(df_monster_class, tag_pre, tag_post,
                      df.names=names(df_monster_class))
}

#' Title
#'
#' @param htm_monster 
#'
#' @return
#' @export
get_monster_list_htm <- function(htm_monster){
    paste0('<div class="monsterList">',htm_monster,'</div>')
}


#' Title
#'
#' @param htm_monster 
#' @param dir_output 
#' @param file_name 
#'
#' @return
#' @export
write_file_htm_monster <- function(htm_monster, dir_output, file_name){
    file_css <- system.file("SFRPG.css", package='sfrpg',
        mustWork=TRUE)
    css <- readChar(file_css, file.info(file_css)$size)

    htm_full <- paste("<!DOCTYPE html>",
        "<html>\r\n<head>\r\n<title>Monster stats</title>",
        "\r\n<style type=\"text/css\">",
        css,
        "</style></head>\r\n<body>",
        monster_htm %>% get_monster_list_htm(),
        "</body></html>",
        sep="\r\n",
        collapse="<br>")
    writeChar(htm_full, file.path(dir_output, file_name))
}