#' Generate html table for powers
#'
#' @param df input dataframe
#' @param pre
#' @param post
#'
#' @return
#' @export
#'
#' @examples
apply_class_power_htm <- function(df, pre, post) {
  if (is.null(df)) {
    return ("")
  }
  df = df %>% dplyr::arrange(
    dplyr::desc(isFeature),
    Type,
    usageColors %>% forcats::fct_relevel('green', 'red', 'gray')
  )
  
  htm <- build_element_apply(
    df,
    pre,
    post,
    df.names = setdiff(names(df), c("Summary", "Build", "usageColors")),
    skipEmpty = TRUE,
    collapse = ' '
  )
  paste("<div class=\"Power-List\">", htm, "</div>" , sep = "")
}


#' Generate html table with power summary
#'
#' @param df input dataframe
#'
#' @return
#' @export
#'
#' @examples
apply_class_power_summary  <- function(df) {
  build_table_apply(
    df,
    df.names = c(
      "Name",
      "Level",
      "Type",
      "UsageLimit",
      "Range",
      "Action",
      "Summary"
    ),
    tableClass = "Power-table"
  )
}

#' Generate html document with class data for all classes
#'
#' @param class Name of class
#' @param build Name of build
#' @param htm_stat string with class stats in html form
#' @param htm_feature string with class features in html form
#' @param htm_power_summary string with html summary table for class powers
#' @param htm_power string with class powers in html form
#'
#' @return string with class section in html form
#' @export
#'
#' @examples
get_class_section <- function(class,
                              build,
                              htm_stat,
                              htm_feature,
                              htm_power_summary,
                              htm_power) {
  class_section = paste(
    "<p><h3>",
    paste(class, build),
    "</h3></p>\r\n",
    "<p><h3>Class Stats</h3></p>\r\n",
    "<p>",
    htm_stat,
    "</p>\r\n",
    "<p><h3>Class features</h3></p>\r\n",
    "<p>",
    htm_feature,
    "</p>\r\n",
    "<p><h3>Class Powers</h3></p>\r\n",
    "<p>",
    htm_power_summary,
    "</p>\r\n",
    "<p>",
    htm_power,
    "</p>\r\n",
    sep = ""
  )
}

#' Generate clean class ctats table from raw data
#'
#' @param df_class_stat_raw class stats table
#'        in raw form
#'
#' @return df_class_stats class stats table in clean form
#' @export
#'
#' @examples
get_df_class_stat <- function(df_class_stat_raw) {
  get_skill_str_column <- function(skill, df_class_stat) {
    df_class_stat[[skill]]  %>%
      tidyr::replace_na('') %>%
      as.character %>%
      stringr::str_replace('0', '') %>%
      stringr::str_replace('1', paste0(skill, ', '))
  }
  
  l_skills = c(
    'Athletics',
    'Authority',
    'Endurance',
    'Concentration',
    'Stealth',
    'Finesse',
    'Perception',
    'Nature',
    'Trickery',
    'Diplomacy',
    'Arcana',
    'Lore'
  )
  
  col_skills = l_skills %>%
    purrr::map_dfc(get_skill_str_column, df_class_stat_raw) %>%
    purrr::transpose() %>%
    purrr::map_chr(paste0, collapse = '') %>%
    stringr::str_replace(', $', '')
  
  df_class_stat_raw %>% dplyr::mutate(Skills = col_skills) %>%
    dplyr::select(-dplyr::one_of(l_skills)) %>%
    dplyr::select(`Class`:`Trained Skills`,
           Skills,
           `Total class skills`:`Expected Armor`)
}


#' Transpose class stats table, into (key, value) form
#'
#' @param df_class_stat
#'
#' @return
#' @export
#'
#' @examples
get_class_stat_trans <- function(df_class_stat) {
  l_keys = df_class_stat %>% names()
  
  get_class_stat_trans_apply <- function(colname, df_class_stat) {
    list(key = colname, value = df_class_stat[[colname]][[1]])
  }
  
  l_keys %>% purrr::map_dfr(get_class_stat_trans_apply, df_class_stat)
  
}

#' Generate dataframe with html text for class rules
#'
#' @param dir_base
#'
#' @return df_class table (Class, Build,  
#'         htm_stat,htm_feature,htm_power_summary,htm_power)
#' @export
#'
#' @examples
get_l_class <- function ()
{
  dir_base  = system.file('raw', "CharacterCreation", package='sfrpg')
  read_my_csv <- function(s, delim = ';') {
    readr::read_delim(
      file.path(dir_base, paste0(s, ".csv")),
      delim = delim,
      col_types = readr::cols(.default = "c")
    )
  }
  
  usageOrder  <- c("", "At-Will", "Encounter", "Daily")
  
  df_class_stat = read_my_csv('Class-stats', delim = ',') %>%
    get_df_class_stat()
  df_class_feature = read_my_csv('Class-features') %>%
    gsub_colwise("\\n", "<br>") %>%
    purrr::map_dfc (tidyr::replace_na, '-')

  df_feature_tag <- read_my_csv('Class-features-tags')
  df_power_tag <-  read_my_csv('Powers-tags')
  df_power_raw <-  read_my_csv('Powers-raw') %>%
    gsub_colwise("\\n", "<br>") %>%
    fillna_df %>%
    dplyr::mutate(usageColors = UsageLimit %>%
             factor %>% 
             forcats::fct_recode(!!!c(
               green = "",
               red = "Encounter",
               gray = "Daily"))
           ) %>%
    dplyr::arrange(Class, isFeature != "Feature",
            Type, usageColors, Level, Name)  %>%
    dplyr::mutate(Name = paste(
      "<span class=\"", usageColors, "\">", Name, sep = "")) %>%
    dplyr::mutate(Level = paste(Level, "</span>", sep = ""))
  
  power_tag_pre <- df_power_tag[1,] %>% dplyr::select(-Class,-Build)
  power_tag_post <- df_power_tag[2,] %>% dplyr::select(-Class,-Build)
  
  feature_tag_pre <- df_feature_tag[1,] %>% dplyr::select(-Class,-Build)
  feature_tag_post <- df_feature_tag[2,] %>% dplyr::select(-Class,-Build)
  
  df_class = list(
    df_power_raw  %>% dplyr::group_by(Class, Build) %>%
      tidyr::nest (.key = 'data_power'),
    df_class_stat %>% dplyr::group_by(Class, Build) %>%
      tidyr::nest (.key = 'data_stat'),
    df_class_feature %>% dplyr::group_by(Class, Build) %>%
      tidyr::nest (.key = 'data_feature')
  ) %>%
  purrr::reduce(dplyr::full_join, by = c('Class', 'Build')) %>%
  dplyr::mutate(
    htm_stat = data_stat %>%
      purrr::map(~ .x %>% get_class_stat_trans) %>%
      purrr::map_chr(
        ~ .x %>% build_table_apply(tableClass = "Class-table", skipHeader = TRUE)
      ),
    htm_feature = data_feature %>% purrr::map_chr(
      build_element_apply,
      feature_tag_pre,
      feature_tag_post,
      skipEmpty = TRUE
    ),
    htm_power = data_power %>% purrr::map_chr(apply_class_power_htm,
                                              power_tag_pre, power_tag_post),
    htm_power_summary = data_power %>% purrr::map_chr(apply_class_power_summary),
    htm_class_section = get_class_section(
      Class,
      Build,
      htm_stat,
      htm_feature,
      htm_power_summary,
      htm_power
      )
    )
  
  df_class
  
}

#' Extract data for a single class build from df_class
#'
#' @param df_class 
#' @param char_class 
#' @param char_build 
#'
#' @return
#' @export
#'
#' @examples
get_class_build <- function(df_class, char_class, char_build) {
  df_class %>% filter(Class == char_class, Build == char_build)
}


#' Add html headers, css data to html string to use in standalone document
#'
#' @param htm_str 
#'
#' @return
#' @export
#'
#' @examples
get_htm_file <- function(str_htm) {

  file_css <- system.file('rmd', "SFRPG.css", package='sfrpg')
  css <- readChar(file_css, file.info(file_css)$size)
  
  paste(
    "<html>\r\n<head>",
    "\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
    css,
    "</style></head>\r\n<body>",
    str_htm,
    "</body></html>",
    sep = "<br/>",
    collapse = ""
  )
}

write_class_file <- function(df_class, char_class, char_build, dir_output) {
  htm_file = get_class_build(
    df_class, char_class, char_build)$htm_class_section %>%
    get_htm_file()
  path = file.path(dir_output,
    paste('1-01-class-',char_class,'-',char_build,'.html'))
  writeLines(htm_file, path)
}

write_class_section_file <- function(df_class, dir_output) {
  htm_file_full = df_class$htm_class_section %>%
    purrr::map_chr( get_htm_file) %>% paste
  writeLines(htm_file_full, file.path(dir_output, '1-01-class-full.html'))
}


