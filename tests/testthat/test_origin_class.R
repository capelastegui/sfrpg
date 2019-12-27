context("Convert data from .csv to html")

#dir_output = here::here('sfrpg', 'tests', 'testthat', 'output')
#print(dir_output)

test_that("read_df_feature works", {
  df_class_feature <- read_df_feature()
  skip_on_travis()
  verify_output(
    test_path('output','test_read_df_class_feature.txt'),
    print(df_class_feature)
  )
})

test_that("read_df_power works", {
  df_class_power <- read_df_power()
  succeed()
})

test_that("get_df_class works", {
  df_class = get_df_class()
  
  skip_on_travis()
  
  verify_output(
    test_path('output','test_class_output.txt'),
    print(df_class)
  )

  test_class_output <- function(class,build){
    htm_file = get_class_build(
    df_class, class, build)$htm_class_section %>%
    get_htm_file()

    verify_output(
      test_path('output',paste0('test_class_',class,'_',build,'.txt')),
      htm_file %>% print()
    )
    htm_file %>% readr::write_file(test_path('output',paste0('test_class_',class,'_',build,'.html')))
  }

  test_class_output('Fighter', 'Guardian')
  test_class_output('Rogue', 'Scoundrel')
  test_class_output('Wizard', 'Elementalist')
  test_class_output('Priest', 'Light')

  #write_class_section_file(df_class, dir_output)
  
  succeed() # Finished
})

test_that("get_df_origin works", {
  df_origin = get_df_origin()
  skip_on_travis()
  verify_output(
    test_path('output','test_origin_output.txt'),
    print(df_origin)
  )

  succeed() # Finished
})

l_color_map=c(green = "", red = "Encounter", gray = "Daily")
