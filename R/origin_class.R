# -- Utility functions

# Convenience function to add <p> tags to an html paragraph
add_p_tags <- function(col) {col %>% build_element('<p>','</p>')}

# Collapse columns in a dataframe into a single character column
collapse_cols <- function(df, collapse=''){

  filter_and_collapse <- function(c){
    c %>% purrr::discard(~nchar(.)==0) %>%
    paste(collapse=collapse)
  }

  df %>%
    purrr::transpose() %>%
    purrr::map_chr(filter_and_collapse)
}

# Add title paragraph column for power block
add_p_title <- function(df_power, usageColors){
  p_title <- df_power %>%
    dplyr::select(Name, Class, isFeature, Level) %>%
    collapse_cols(' ')

  df_power %>%
    dplyr::mutate(
      p_title = paste0("<span class=\"",
                       usageColors,
                       " large\"><strong>",
                       p_title,
                       '</strong></span>'))
}

# Add type paragraph column for power block
add_p_type <- function(df_power){
  p_type <- df_power %>%
    dplyr::select(Type, UsageLimit, UsageNumber,Keywords) %>%
    collapse_cols(' ')

  df_power %>% dplyr::mutate(p_type=p_type)
}

# Add action paragraph column for power block
add_p_action <- function(df_power) {
  p_action = df_power %>%
  dplyr::select(Action, Trigger, Range, Target, AttackRoll)  %>%
    collapse_cols(' ')

  df_power %>% dplyr::mutate(p_action=p_action)
}


# -- Data processing functions

#' #' Read class power data from partial .csv tables
#'
#' @return merged class power table, raw
#' @export
read_df_power <- function(is_class=TRUE) {

  df_class_build <- read_my_csv('build')
  df_powers_map <- read_my_csv('power_map')
  df_powers_raw <- read_my_csv('powers')

    df_powers = df_class_build %>%
    dplyr::inner_join(df_powers_map) %>%
    dplyr::inner_join(df_powers_raw) %>%
    dplyr::select (-build_id,-power_id)

  df_powers
}


#' @export
clean_df_feature <- function(df_feature_raw, df_feature_tag=NULL) {

  if (df_feature_tag %>% is.null())  {
    df_feature_tag <- read_my_csv('feature_tags')
  }

  feature_tag_pre <- df_feature_tag[1,] %>% dplyr::select(-Class,-Build)
  feature_tag_post <- df_feature_tag[2,] %>% dplyr::select(-Class,-Build)

  htm_feature <- df_feature_raw %>%
    build_element_apply(
    feature_tag_pre,
    feature_tag_post,
    skipEmpty = TRUE
  )

  htm_feature
}

#' Build power block from raw data
#'
#' @param df_power_raw
#' @param df_power_tag
#' @param character_sheet boolean, true if generating table for a character
#'        sheet
#'
#' @return character vector with html power blocks
#' @export
clean_df_power <- function(df_power_raw, df_power_tag=NULL, character_sheet=FALSE)
{
  l_color_map=c(green = "", red = "Encounter", gray = "Daily")

  if (df_power_tag %>% is.null()) {df_power_tag <-  read_my_csv('power_tags')}

  power_tag_pre <- df_power_tag[1,] %>% dplyr::select(-Class,-Build)
  power_tag_post <- df_power_tag[2,] %>% dplyr::select(-Class,-Build)

  df_power <- df_power_raw  %>%
    gsub_colwise("\\r\\n", "<br>") %>%
    fillna_df %>%
    dplyr::mutate(
      usageColors = UsageLimit %>%
        factor %>%
        # Assign colors as factor levels
        # Noisy function, suppressing warnings
        #forcats::fct_recode(!!!l_color_map) %>%
        {suppressWarnings( forcats::fct_recode(.,!!!l_color_map))} %>%
        # Reorder factor levels: green,red,gray
        # Noisy function, suppressing warnings
        #forcats::fct_relevel(c('green','red','gray'))
        {suppressWarnings( forcats::fct_relevel(.,c('green','red','gray')))}
    ) %>%
    dplyr::arrange(isFeature != "Feature",
                   Type, usageColors, Level, Name)

  build_cols <- setdiff(names(df_power),
    c("Summary", "Build", "usageColors"))

  power_block_tmp <- df_power %>%
    build_element_apply(power_tag_pre, power_tag_post,
      build_cols, reduce=FALSE) %>%
    add_p_title(df_power$usageColors) %>%
    add_p_type() %>%
    add_p_action() %>%
    dplyr::select(p_title, p_type, p_action, EffectPre, Hit,
           Miss, EffectPost, Misc, SecondaryAttack, Upgrades) %>%
    purrr::map_dfc(add_p_tags)

  if(character_sheet){
    power_block_tmp <- power_block_tmp %>%
    dplyr::select(-Upgrades) %>%
    # There may be duplicate entries for powers mapped to multiple builds
    dplyr::distinct()
  }
  power_block = power_block_tmp %>%
    collapse_cols('\r\n') %>%
    build_element('<div class="Power">','</div>')

  if(character_sheet) {
    df_power_clean_tmp <- data.frame(
      Class='sheet', Build='sheet', power_block=power_block
    )
  } else {
    df_power_clean_tmp <- df_power %>%
    dplyr::select(Class, Build) %>%
    dplyr::mutate(power_block = power_block)
  }

  df_power_clean <- df_power_clean_tmp  %>%
    dplyr::group_by(Class, Build) %>%
    dplyr::summarise(htm_power=power_block %>%
      paste(collapse='\r\n')) %>%
    dplyr::mutate(htm_power = paste0(
      '<div class="Power-List">',htm_power, '</div>'))
  
  df_power_clean
}


#' Generate html table with power summary
#'
#' @param df input dataframe
#'
#' @return
#' @export
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
get_class_section <- function(class,
                              build,
                              htm_stat,
                              htm_feature,
                              #htm_power_summary,
                              htm_power,
                              is_class=TRUE) {
  str_type = ifelse(is_class, 'Class', 'Origin')

  class_section = paste0(
    "<p><h3>",
    paste(class, build),
    "</h3></p>\r\n",
    "<p><h3>",str_type," Stats</h3></p>\r\n",
    "<p>",
    htm_stat,
    "</p>\r\n",
    "<p><h3>",str_type," features</h3></p>\r\n",
    "<p>",
    htm_feature,
    "</p>\r\n",
    "<p><h3>",str_type," Powers</h3></p>\r\n",
    #"<p>",
    #htm_power_summary,
    #"</p>\r\n",
    "<p>",
    htm_power,
    "</p>\r\n"
  )
}




#' Generate clean class stats table from raw data
#'
#' @param df_class_stat_raw class stats table
#'        in raw form
#'
#' @return df_class_stats class stats table in clean form
#' @export
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


#' Read origin stats data from partial .csv tables
#'
#' @return
#' @export
read_df_origin_stats <- function(){
  df_origin_build <- read_my_csv('build') %>% dplyr::filter(type=='origin')
  df_origin_stats_partial <- read_my_csv('origin_stats')

  df_origin_stats_raw <- df_origin_build %>%
    dplyr::inner_join(df_origin_stats_partial)

  df_origin_stat <- df_origin_stats_raw %>%
    dplyr::select(-build_id, -type) %>%
    dplyr::group_by(Class, Build) %>%
    dplyr::group_nest (.key = 'data_stat_tmp') %>%
    dplyr::mutate(data_stat = data_stat_tmp %>%
           purrr::map(trans_df)) %>%
    dplyr::select(Class, Build, data_stat)

  df_origin_stat
}



#' Read class feature data from partial .csv tables
#'
#' @return
#' @export
read_df_feature <- function(is_class=TRUE){

   df_class_build <- read_my_csv('build')
   df_class_features_map <- read_my_csv('feature_map')
   df_class_features <- read_my_csv('features')

   df_features = df_class_build %>%
    dplyr::inner_join(df_class_features_map) %>%
    dplyr::inner_join(df_class_features) %>%
    dplyr::select(-build_id, -feature_id)

  df_features
}


#' @export
get_df_feature_from_sheet <- function(
  l_feature_id, Class='sheet', Build='sheet'){
  df_features  <- read_my_csv('features') %>%
    dplyr::filter(feature_id %in% l_feature_id) %>%
    dplyr::mutate(Class=Class, Build = Build)  %>%
    dplyr::select(-feature_id)

  df_features
}



#' @export
get_df_power_from_sheet <- function(
l_power_id, Class='sheet', Build='sheet')
{
  df_powers_raw  <- read_my_csv('powers') %>%
    dplyr::filter(power_id %in% l_power_id)

    df_class_build <- read_my_csv('build')
    df_powers_map <- read_my_csv('power_map')

    df_powers = df_class_build %>%
    dplyr::inner_join(df_powers_map) %>%
    dplyr::inner_join(df_powers_raw) %>%
    dplyr::select (-build_id,-power_id)

  df_powers
}

#' Generate dataframe with html text for combat maneuver rules
#'
#' @param dir_base
#'
#' @return df_class table (Class, Build,
#'         htm_stat,htm_feature,htm_power_summary,htm_power)
#' @export
get_df_maneuver <- function(){
    df_power_raw = read_df_power() %>%
    dplyr::filter(type=='maneuver')%>%
    dplyr::select(-type)

    df_power_clean = df_power_raw %>%  clean_df_power()

    df_power_clean

}

# TODO: option not to include df_class_stat

#' Generate dataframe with html text for class rules
#'
#' @param dir_base
#'
#' @return df_class table (Class, Build,  
#'         htm_stat,htm_feature,htm_power_summary,htm_power)
#' @export
get_df_class <- function (
  df_class_feature_raw = NULL,
  df_class_power_raw = NULL) {
  usageOrder  <- c("", "At-Will", "Encounter", "Daily")
  
  df_class_stat = read_my_csv('class_stats', delim = ',') %>%
    get_df_class_stat()

  if (df_class_feature_raw %>% is.null()) {
    df_class_feature_raw = read_df_feature() %>%
      dplyr::filter(type=='class')%>%
      dplyr::select(-type)
  }

    if (df_class_power_raw %>% is.null()) {
    df_power_raw = read_df_power() %>%
      dplyr::filter(type=='class')%>%
      dplyr::select(-type)
  }

  df_class_feature = df_class_feature_raw %>%
    gsub_colwise("\\r\\n", "<br>") %>%
    purrr::map_dfc (tidyr::replace_na, '-')

  df_power_clean = df_power_raw %>% clean_df_power()
  
  df_class = list(
    df_power_clean,
    df_class_stat %>% dplyr::group_by(Class, Build) %>%
      dplyr::group_nest (.key = 'data_stat'),
    df_class_feature %>% dplyr::group_by(Class, Build) %>%
      dplyr::group_nest (.key = 'data_feature')
  ) %>%
  purrr::reduce(dplyr::full_join, by = c('Class', 'Build')) %>%
  dplyr::mutate(
        htm_stat = data_stat %>%
          purrr::map(~ .x %>% trans_df) %>%
          purrr::map_chr(
            ~ .x %>% build_table_apply(tableClass = "Class-table", skipHeader = TRUE)
            ),
        htm_feature = data_feature %>% purrr::map_chr(clean_df_feature),
        htm_class_section = get_class_section(
          Class,
          Build,
          htm_stat,
          htm_feature,
          htm_power
          )
    )
  
  df_class
  
}


#' Generate dataframe with html text for origin rules
#'
#' @param dir_base
#'
#' @return df_origin table (Class, Build,
#'         htm_stat,htm_feature,htm_power_summary,htm_power)
#' @export
get_df_origin <- function (
  df_feature_raw = NULL,
  df_power_raw = NULL) {
  df_origin_stats = read_df_origin_stats()

  if (df_feature_raw %>% is.null()) {
    df_feature_raw = read_df_feature(is_class=FALSE) %>%
    dplyr::filter(type=='origin') %>%
      dplyr::select(-type)
  }

    if (df_power_raw %>% is.null()) {
    df_power_raw = read_df_power(is_class=FALSE) %>%
    dplyr::filter(type=='origin') %>%
      dplyr::select(-type)
  }

  df_feature = df_feature_raw %>%
    gsub_colwise("\\r\\n", "<br>") %>%
    purrr::map_dfc (tidyr::replace_na, '-')

  df_power_clean = df_power_raw %>%  clean_df_power()

  df_origin = list(
    df_power_clean,
    df_origin_stats,
    df_feature %>% dplyr::group_by(Class, Build) %>%
      dplyr::group_nest (.key = 'data_feature')
  ) %>%
  purrr::reduce(dplyr::full_join, by = c('Class', 'Build')) %>%
  dplyr::mutate(
        htm_stat = data_stat  %>%
          purrr::map_chr(
            ~ .x %>% build_table_apply(tableClass = "Class-table", skipHeader = TRUE)
            ),
        htm_feature = data_feature %>% purrr::map_chr(clean_df_feature),
        htm_class_section = get_class_section(
          Class,
          Build,
          htm_stat,
          htm_feature,
          htm_power,
          is_class=FALSE
          )
    )

  df_origin

}



# TODO: option not to include df_class_stat

#' Generate dataframe with html text for character sheet
#'
#' @param dir_base
#'
#' @return df_class table (Class, Build,
#'         htm_stat,htm_feature,htm_power_summary,htm_power)
#' @export
get_html_sheet <- function (l_feature_id, l_power_id,
character_name='Character Sheet') {
  df_power_raw = get_df_power_from_sheet(l_power_id)

  df_class_feature = get_df_feature_from_sheet(l_feature_id) %>%
    gsub_colwise("\\r\\n", "<br>") %>%
    purrr::map_dfc (tidyr::replace_na, '-')

  htm_power = (df_power_raw %>% clean_df_power(character_sheet=TRUE))$htm_power

  htm_feature = df_class_feature %>% select(Name, Description) %>%
    clean_df_feature()

  htm_sheet = paste(
    "<p><h3>",
    character_name,
    "</h3></p>\r\n",
    "<p><h3>Class features</h3></p>\r\n",
    "<p>",
    htm_feature,
    "</p>\r\n",
    "<p><h3>Class Powers</h3></p>\r\n",
    "<p>",
    htm_power,
    "</p>\r\n",
    sep = ""
  )
  htm_sheet
}

#' Extract data for a single class build from df_class
#'
#' @param df_class 
#' @param char_class 
#' @param char_build 
#'
#' @return
#' @export
get_class_build <- function(df_class, char_class, char_build) {
  df_class %>% dplyr::filter(Class == char_class, Build == char_build)
}




#' Write class data for a single class into an html file
#'
#' @param df_class 
#' @param char_class 
#' @param char_build 
#' @param dir_output 
#'
#' @return
#' @export
write_class_file <- function(df_class, char_class, char_build, dir_output) {
  htm_file = get_class_build(
    df_class, char_class, char_build)$htm_class_section %>%
    get_htm_file()
  path = file.path(dir_output,
    paste0('1-01-class-',char_class,'-',char_build,'.html'))
  print(paste('Writing class file: ', char_class, char_build, 'to:', path))
  writeLines(htm_file, path)
}

write_html_sheet <- function(l_feature_id, l_power_id,
                             dir_output, character_name) {
  htm_file = get_html_sheet(l_feature_id, l_power_id, character_name) %>%
      get_htm_file()
    path = file.path(dir_output,
      paste0('sheet_',character_name,'.html'))
  writeLines(htm_file, path)
}


#' Write class data for all classes into an html file
#'
#' @param df_class 
#' @param dir_output 
#'
#' @return
#' @export
write_class_section_file <- function(df_class, dir_output) {
  htm_file_full = df_class$htm_class_section %>%
    purrr::map_chr( get_htm_file) %>% paste
  writeLines(htm_file_full, file.path(dir_output, '1-01-class-full.html'))
}



# Add title paragraph column for item block
#' @export
add_p_item_title <- function(df_item, rarity_colors){
  p_title <- df_item %>%
    dplyr::select(name, type, rarity, level_h, level_p, level_e) %>%
    collapse_cols(' ')

  df_item %>%
    dplyr::mutate(p_title = paste(
      "<span class=\"", rarity_colors, " large\"><strong>",
      p_title, '</strong></span>',sep = ""))
}

#' Write magic items data
#'
#' @param df_items_raw
#' @param df_items_tag
#'
#' @return
#' @export
clean_df_items <- function(df_items_raw=NULL, df_items_tag=NULL) {

  l_color_map=c(black = "Common", silver = "Uncommon", gold = "Rare")

  if (df_items_raw %>% is.null())  {
    df_items_raw <- read_my_csv('magic_items')
  }

  if (df_items_tag %>% is.null())  {
    df_items_tag <- read_my_csv('item_tags')
  }

  df_items <- df_items_raw %>%
    fillna_df %>%
    dplyr::mutate(
      rarity_colors = rarity %>%
        factor %>%
        # Assign colors as factor levels
        # Noisy function, suppressing warnings
        #forcats::fct_recode(!!!l_color_map) %>%
        {suppressWarnings( forcats::fct_recode(.,!!!l_color_map))} %>%
        # Reorder factor levels: green,red,gray
        # Noisy function, suppressing warnings
        #forcats::fct_relevel(c('green','red','gray'))
        {suppressWarnings( forcats::fct_relevel(.,c('black','gray','gold')))}
    ) %>%
    dplyr::arrange(type, rarity, level_h, name)

  df_items

  items_tag_pre <- df_items_tag[1,] #%>% dplyr::select(-Class,-Build)
  items_tag_post <- df_items_tag[2,] #%>% dplyr::select(-Class,-Build)

  build_cols <- setdiff(names(df_items),
    c("Summary", "Build", "usageColors"))

  item_block_tmp <- df_items %>%
    build_element_apply(items_tag_pre, items_tag_post,
      build_cols, reduce=FALSE) %>%
      add_p_item_title() %>%
      dplyr::select(p_title, p1, p2, p3, p4, p5) %>%
      purrr::map_dfc(add_p_tags)

  item_block <- item_block_tmp %>%
    collapse_cols('\r\n') %>%
    build_element('<div class="Power">','</div>') %>%
    paste(collapse='\r\n')

  htm_item <- paste0('<div class="Power-List">',item_block, '</div>')

  htm_item
}

write_items_file <- function(htm_items, dir_output) {
  writeLines(htm_items, file.path(dir_output, 'magic_items.html'))
}
