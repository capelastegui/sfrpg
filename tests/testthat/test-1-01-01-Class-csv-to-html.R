context("Convert data from .csv to html")

library(plyr)
library(dplyr)
library(purrr)

dir_base=file.path(here::here(),'sfrpg')
print(dir_base)

dir_output = file.path(dir_base, 'tests', 'testthat', 'output')
file_css <- file.path(dir_base,"Rmd","SFRPG.css")
css <- readChar(file_css, file.info(file_css)$size)


test_that("get_equip_tables works", {
  l_class = get_l_class(dir_base)
  
  str_tables = l_class %>% purrr::flatten () %>% paste0(collapse='<br/>')
  
  str_tables <- paste("<html>\r\n<head>\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
                      css,
                      "</style></head>\r\n<body>",
                      str_tables,
                      "</body></html>",
                      sep="<br/>",
                      collapse="")
  
  writeLines(str_tables, file.path(dir_output, '1-01-class.html'))
  
  str_fighter = paste("<html>\r\n<head>\r\n<title>power-test</title>\r\n<style type=\"text/css\">",
                      css,
                      "</style></head>\r\n<body>",
                      "<p>",l_class$Fighter$Guardian$stats.htm,"</p>",
                      "<p>",l_class$Fighter$Guardian$powers.table,"</p>",
                      l_class$Fighter$Guardian$powers.htm,
                      "</body></html>",
                      sep="<br/>",
                      collapse="")
  
  writeLines(str_fighter, file.path(dir_output, '1-01-class-fighter.html'))
  

})

