context("Convert data from .csv to html")

test_that("build_element works", {
  # Test 1 - simple input
  str_result = build_element(c('hello','world'),'<<', '>>')
  str_expected = c('<<hello>>', '<<world>>')
  expect_equal(str_result, str_expected)
  
  # Test 2 - missing values, skipEmpty=FALSE
  str_result = build_element(c('hello',NA, '', 'world'),'<<', '>>', skipEmpty=FALSE)
  str_expected = c('<<hello>>', '<<NA>>', '<<>>','<<world>>')
  expect_equal(str_result, str_expected)
  
  # Test 3 - missing values, skipEmpty=TRUE
  str_result = build_element(c('hello',NA,'', 'world'),'<<', '>>', skipEmpty=TRUE)
  str_expected = c('<<hello>>', '', '', '<<world>>')
  expect_equal(str_result, str_expected)
  
  #str_result1 %>% print
  #succeed("Test implementation in progress")
})

test_that("build_element_apply works", {
  df_pre = tibble::tibble(x='<x>', y='<y>' ,Body='(')
  df_post = tibble::tibble(x= '<x>', y='</y>', Body=')')

  # Test 1 - simple input
  str_result = build_element_apply(
    tibble::tibble(x=c('hello','world'),y=c('1', '2')), df_pre, df_post)
  str_expected = "\r\n(<x>hello<x><y>1</y>)\r\n(<x>world<x><y>2</y>)"
  expect_equal(str_result, str_expected)

  # Test 2 - missing values, skipEmpty=TRUE
  str_result = build_element_apply(
    tibble::tibble(x=c('hello',NA,"",'world'),y=c('1',NA,"", '2')), df_pre, df_post)
  str_expected = "\r\n(<x>hello<x><y>1</y>)\r\n()\r\n()\r\n(<x>world<x><y>2</y>)"
  expect_equal(str_result, str_expected)
  #succeed("Test implementation in progress")
  
  # Test 3 - missing values in df_pre, df_post, skipEmpty=TRUE
  df_pre_na = tibble::tibble(x='<x>', y=NA ,Body='(')
  df_post_na = tibble::tibble(x= '<x>', y=NA, Body=')')
  str_result = build_element_apply(
    tibble::tibble(x=c('hello',NA,"",'world'),y=c('1',NA,"", '2')), df_pre_na, df_post_na)
  str_expected = "\r\n(<x>hello<x>1)\r\n()\r\n()\r\n(<x>world<x>2)"
  expect_equal(str_result, str_expected)
  #succeed("Test implementation in progress")
})

test_that("build_table_apply works", {
  df_in = tibble::tibble(x=c('hello','world'),y=c('1', '2')) 
  
  # Test 1 - simple input
  str_result = build_table_apply(df_in)
  str_expected = paste0(
    "<table> \r\n",
    "<tbody> <tr><td>x</td><td>y</td></tr>\n",
    "<tr><td>hello</td><td>1</td></tr>\n",
    "<tr><td>world</td><td>2</td></tr>\n ",
    "</tbody>\r\n</table> \n")
  expect_equal(str_result, str_expected)
  
  # Test 2 - with df.names and tableClass
  str_result = build_table_apply(df_in, df.names='x', tableClass='my_class')
  str_expected = paste0(
    "<table class=\"my_class\"> \r\n",
    "<tbody> <tr><td>x</td></tr>\n",
    "<tr><td>hello</td></tr>\n",
    "<tr><td>world</td></tr>\n ",
    "</tbody>\r\n</table> \n")
  expect_equal(str_result, str_expected)
  
  # Test 3 - skipHeader=TRUE
  str_result = build_table_apply(df_in, skipHeader=TRUE)
  str_expected = paste0(
    "<table> \r\n",
    "<tbody> <tr><td>hello</td><td>1</td></tr>\n",
    "<tr><td>world</td><td>2</td></tr>\n ",
    "</tbody>\r\n</table> \n")
  expect_equal(str_result, str_expected)
  
})

test_that('gsub_colwise works',{
  df_in = tibble::tibble(x=c('hello','world'),y=c('a', 'o')) 
  df_result = df_in %>% gsub_colwise('o','u')
  df_expected = tibble::tibble(x=c('hellu','wurld'),y=c('a', 'u')) 
  expect_equal(df_result, df_expected)
  
})

test_that('refactor works',{
  df_in = tibble::tibble(x=c('hello','world'),y=c('a', 'o')) %>% 
    dplyr::mutate(x=x %>% factor)
  df_result = df_in %>% head(1) %>% refactor
  df_expected = tibble::tibble(x=c('hello'),y=c('a')) %>% 
    dplyr::mutate(x=x %>% factor) %>% as.data.frame()
  expect_equal(df_result, df_expected)
  
})
