context("Convert data from .csv to html")

library(plyr)
library(dplyr)
library(purrr)

dir_base=file.path(here::here(),'sfrpg')
print(dir_base)

dir_output = file.path(dir_base, 'tests', 'testthat', 'output')
file_css <- file.path(dir_base,"Rmd","SFRPG.css")
css <- readChar(file_css, file.info(file_css)$size)

get_htm_file <- function(htm_str) {
  dir_base=file.path(here::here(),'sfrpg')
  file_css <- file.path(dir_base,"Rmd","SFRPG.css")
  css <- readChar(file_css, file.info(file_css)$size)
  
  paste(
    "<html>\r\n<head>",
    "\r\n<title>Test tables</title>\r\n<style type=\"text/css\">",
    css,
    "</style></head>\r\n<body>",
    htm_str,
    "</body></html>",
    sep="<br/>",
    collapse="")
}


test_that("get_l_class works", {
  df_class = get_l_class(dir_base)
  
  print(df_class)
  
  write_class_file <- function(char_class, char_build) {
    
    htm_file = get_class_build(df_class, char_class, char_build)$htm_class_section %>% 
      get_htm_file()
    path = file.path(dir_output, paste('1-01-class-',char_class,'-',char_build,'.html'))
    writeLines(htm_file, path)
  }
  
  write_class_file('Fighter', 'Guardian')
  write_class_file('Rogue', 'Scoundrel')
  write_class_file('Wizard', 'Elementalist')
  write_class_file('Priest', 'Light')
  

  htm_file_full = df_class$htm_class_section %>% map_chr( get_htm_file) %>% paste
  
  writeLines(htm_file_full, file.path(dir_output, '1-01-class-full.html'))
  
})

