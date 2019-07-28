context("Convert data from .csv to html")

test_that("buildElement works", {
  str_result1 = buildElement(c('hello','world'),'<<', '>>')
  str_result1 %>% print
  
  
  succeed("Test implementation in progress")
})
