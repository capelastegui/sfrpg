context("Convert data from .csv to html")

#dir_output = here::here('tests', 'testthat', 'output')
file_css <- system.file("SFRPG_test.css", package='sfrpg',
     mustWork=TRUE)
css <- readChar(file_css, file.info(file_css)$size)

test_that("get_equip_tables works", {
  df_equip <- get_df_equip()
  expect_equal(
    df_equip$table_type %>% unique(),
    c('Weapons','Armor','Armor_by_Class','Implements'))
  expect_equal(
    df_equip$Training %>% unique(),
    c( 'Improvised','Basic', 'Martial', 'Inner',  '-'))
  
  str_tables =df_equip$table %>% paste0(collapse='\r\n')
  
  str_tables <- paste("<html>\r\n<head>\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
                     css,
                     "</style></head>\r\n<body>",
                     str_tables,
                     "</body></html>",
                     sep="\r\n",
                     collapse="")
  skip_on_travis()
  verify_output(
    test_path('output','test_equip_output.txt'),
    print(df_equip)
  )
  succeed()
  
  #writeLines(str_tables, file.path(dir_output, '1-01-equiptables.html'))
})

