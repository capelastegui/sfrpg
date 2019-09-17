context("Convert data from .csv to html")

# library(dplyr)
# library(purrr)

dir_base=file.path(here::here(),'sfrpg')
print(dir_base)

dir_output = file.path(dir_base, 'tests', 'testthat', 'output')
file_css <- file.path(dir_base,"Rmd","SFRPG.css")
css <- readChar(file_css, file.info(file_css)$size)


test_that("get_equip_tables works", {
  df_tables_equip <- get_equip_tables(dir_base)
  expect_equal(
    df_tables_equip$table_type %>% unique(),
    c('Weapons','Armor','Armor_by_Class','Implements'))
  expect_equal(
    df_tables_equip$Training %>% unique(),
    c( 'Improvised','Basic', 'Martial', 'Inner',  '-'))
  
  str_tables =df_tables_equip$table %>% paste0(collapse='\r\n')
  
  str_tables <- paste("<html>\r\n<head>\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
                     css,
                     "</style></head>\r\n<body>",
                     str_tables,
                     "</body></html>",
                     sep="\r\n",
                     collapse="")
  
  writeLines(str_tables, file.path(dir_output, '1-01-equiptables.html'))
})

