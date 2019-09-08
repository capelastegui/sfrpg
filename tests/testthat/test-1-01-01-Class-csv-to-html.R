context("Convert data from .csv to html")

# library(dplyr)
# library(purrr)

dir_base=file.path(here::here(),'sfrpg')
print(dir_base)

dir_output = file.path(dir_base, 'tests', 'testthat', 'output')
file_css <- file.path(dir_base,"Rmd","SFRPG.css")
css <- readChar(file_css, file.info(file_css)$size)


test_that("get_equip_tables works", {
  l_tables_equip = get_equip_tables(dir_base)
  expect_equal(
    l_tables_equip %>% names(),
    c('weapons','armor','legacy.class.armor','implements'))
  expect_equal(
    l_tables_equip$weapons %>% names(),
    c('Basic', 'Improvised', 'Inner', 'Martial'))
  
  str_tables =l_tables_equip %>% purrr::flatten () %>% paste0(collapse='\r\n')
  
  str_tables <- paste("<html>\r\n<head>\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
                     css,
                     "</style></head>\r\n<body>",
                     str_tables,
                     "</body></html>",
                     sep="\r\n",
                     collapse="")
  
  writeLines(str_tables, file.path(dir_output, '1-01-equiptables.html'))
})

