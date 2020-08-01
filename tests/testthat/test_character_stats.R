context("Generate character stat tables")

test_that("get_df_hp works", {
  df_result <- get_df_hp()
  succeed()
})

test_that("get_df_hp_beast works", {
  df_result <- get_df_hp_beast()
  succeed()
})

test_that("get_df_hp_html works", {
  df_result <- get_df_hp_html()
  succeed()
})

test_that("get_df_hp_html_beast works", {
  df_result <- get_df_hp_html_beast()
  succeed()
})
