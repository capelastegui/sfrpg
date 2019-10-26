context("Convert data from .csv to html")

#dir_output = here::here('tests', 'testthat', 'output')
file_css <- system.file('rmd', "SFRPG.css", package='sfrpg')
css <- readChar(file_css, file.info(file_css)$size)

test_that("get_equip_tables works", {
  df_tables_equip <- get_equip_tables()
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
  
  print(df_tables_equip)
  succeed()
  
  #writeLines(str_tables, file.path(dir_output, '1-01-equiptables.html'))
})

