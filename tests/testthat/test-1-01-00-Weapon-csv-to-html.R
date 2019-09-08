context("Convert data from .csv to html")

library(plyr)
library(dplyr)
library(purrr)

basedir=file.path(here::here(),'sfrpg')
print(basedir)

dir_output = file.path(basedir, 'tests', 'testthat', 'output')



test_that("getEquipmentTables works", {
  l_tables_equip = getEquipmentTables(basedir)
  expect_equal(
    l_tables_equip %>% names(),
    c('weapons','armor','legacy.class.armor','implements'))
  expect_equal(
    l_tables_equip$weapons %>% names(),
    c('Basic', 'Improvised', 'Inner', 'Martial'))
  
  str_tables =l_tables_equip %>% purrr::flatten () %>% paste0(collapse='\r\n')
  writeLines(str_tables, file.path(dir_output, '1-01-equiptables.html'))
})

