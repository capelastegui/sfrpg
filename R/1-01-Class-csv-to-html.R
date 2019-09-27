apply_class_power_htm <- function(df, pre, post) {
    if (is.null(df)){return ("")}
    df = df %>% arrange(desc(isFeature), Type, usageColors %>% forcats::fct_relevel('green','red','gray'))
  
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

get_class_section <- function(class, build,
                              htm_stat,htm_feature, htm_power_summary, htm_power) {
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

get_l_class <- function (dir_base = here::here())
{
  #require(rutils)
  source(file.path(dir_base, "R", "0-00-csv-to-html.R"))
  source(file.path(dir_base, "R", "utils.R"))
  
  read_my_csv <- function(s) {
    readr::read_delim(
      file.path(dir_base, "raw", "CharacterCreation", paste0(s, ".csv")),
      delim = ";",
      col_types = readr::cols(.default = "c")
    )
  }
  

  usageOrder  <- c("", "At-Will", "Encounter", "Daily")

df_class_stat = read_my_csv('Class-stats') # TODO: header=TRUE, if required
df_class_feature = read_my_csv('Class-features') %>%
  gsub_colwise("\\n", "<br>") %>%
  map_dfc (tidyr::replace_na,'-')
# Todo: convert columns to character if required
df_feature_tag <- read_my_csv('Class-features-tags')
df_power_tag <-  read_my_csv('Powers-tags')
df_power_raw <-  read_my_csv('Powers-raw') %>%
  gsub_colwise("\\n", "<br>") %>%
  fillna_df %>%
  mutate(usageColors = UsageLimit %>%
           factor %>% forcats::fct_recode(!!!c(
             green = "",
             red = "Encounter",
             gray = "Daily"
           ))) %>%
  arrange(Class, isFeature != "Feature", Type, usageColors, Level, Name)  %>%
  mutate(Name = paste("<span class=\"", usageColors, "\">", Name, sep =
                        "")) %>%
  mutate(Level = paste(Level, "</span>", sep = ""))

power_tag_pre <- df_power_tag[1, ] %>% select(-Class, -Build)
power_tag_post <- df_power_tag[2, ] %>% select(-Class, -Build)

feature_tag_pre <- df_feature_tag[1, ] %>% select(-Class, -Build)
feature_tag_post <- df_feature_tag[2, ] %>% select(-Class, -Build)

df_class = list(
  df_power_raw  %>% group_by(Class, Build) %>% tidyr::nest (.key='data_power'),
  df_class_stat %>% group_by(Class, Build) %>% tidyr::nest (.key='data_stat'),
  df_class_feature %>% group_by(Class, Build) %>% tidyr::nest (.key='data_feature')
) %>%
  reduce(full_join, by= c('Class', 'Build')) %>%
  mutate(
    htm_stat =data_stat %>%  map_chr(
      ~ .x %>% trans_df2() %>% build_table_apply,  # TODO: convert to tibble, set column names
      tableClass = "Class-table", skipHeader = TRUE),
    htm_feature = data_feature %>% map_chr(
      build_element_apply,
      feature_tag_pre, feature_tag_post, skipEmpty = TRUE),
    htm_power = data_power %>% map_chr(apply_class_power_htm,
        power_tag_pre, power_tag_post),
    htm_power_summary = data_power %>% map_chr(apply_class_power_summary),
    htm_class_section = get_class_section(
      Class, Build, htm_stat, htm_feature, htm_power_summary, htm_power
    )
    )

df_class

}

get_class_build <- function(df_class, char_class, char_build) {
  df_class %>% filter(Class==char_class, Build==char_build)
}


get_htm_file <- function(htm_str) {
  dir_base=file.path(here::here(),'sfrpg')
  file_css <- file.path(dir_base,"Rmd","SFRPG.css")
  css <- readChar(file_css, file.info(file_css)$size)

  paste(
  "<html>\r\n<head>",
  "\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
  css,
  "</style></head>\r\n<body>",
  str_htm,
  "</body></html>",
  sep="<br/>",
  collapse="")
}


writeClassList  <- function(l_class)
{
  # TODO: change workflow to purrr nested data, list-column
  file_css <- file.path(dir_base, "Rmd", "SFRPG.css")
  
  file_class_stat_htm  <-
    file.path(dir_base, "html", "CharacterCreation", "Class-stats.html")
  
  file_power_htm <-
    file.path(dir_base, "html", "CharacterCreation", "Powers.html")
  file_power_table_htm <-
    file.path(dir_base, "html", "CharacterCreation", "Powers-table.html")

  #Build full text descriptions
  power.full <-
    paste(
      "<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
      css,
      "</style></head>\r\n<body>",
      "<p>",
      l_class$Fighter$Guardian$stats.htm,
      "</p>",
      "<p>",
      l_class$Fighter$Guardian$powers.table,
      "</p>",
      l_class$Fighter$Guardian$powers.htm,
      "</body></html>",
      sep = "\r\n",
      collapse = ""
    )
  
  tmp <- llply.n(
    l_class,
    2,
    .fun = function(l) {
      paste(
        "<p>",
        l$classbuild,
        "</p>\r\n",
        "<p><h3>Class Stats</h3></p>\r\n",
        "<p>",
        l$stats.htm,
        "</p>\r\n",
        "<p><h3>Class features</h3></p>\r\n",
        "<p>",
        l$features.htm,
        "</p>\r\n",
        "<p><h3>Class Powers</h3></p>\r\n",
        "<p>",
        l$powers.htm,
        "</p>\r\n",
        sep = ""
      )
    }
  )
  
  tmp <- paste(llply(tmp, collapse = "", .fun = paste), collapse = "\r\n")
  
  power.full <-
    paste(
      "<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
      css,
      "</style></head>\r\n<body>",
      tmp,
      "</body></html>",
      sep = "",
      collapse = ""
    )
  
  writeChar(power.full, file_power_htm)
  classList
}
