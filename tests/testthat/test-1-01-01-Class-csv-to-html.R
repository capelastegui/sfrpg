context("Convert data from .csv to html")

dir_base=file.path(here::here(),'sfrpg')
print(dir_base)

dir_output = file.path(dir_base, 'tests', 'testthat', 'output')

test_that("get_l_class works", {
  df_class = get_l_class(dir_base)
  
  print(df_class)
  
  write_output <- function(class, build) {
    write_class_file(df_class, class, build, dir_output)
  }
  
  write_output('Fighter', 'Guardian')
  write_output('Rogue', 'Scoundrel')
  write_output('Wizard', 'Elementalist')
  write_output('Priest', 'Light')

  write_class_section_file(df_class, dir_output)
  
  succeed() # Finished
})

