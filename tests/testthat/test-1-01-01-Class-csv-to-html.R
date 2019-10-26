context("Convert data from .csv to html")

#dir_output = here::here('sfrpg', 'tests', 'testthat', 'output')
#print(dir_output)

test_that("get_l_class works", {
  df_class = get_l_class()
  
  print(df_class)
  
  write_output <- function(class, build) {
    write_class_file(df_class, class, build, dir_output)
  }
  
  #write_output('Fighter', 'Guardian')
  #write_output('Rogue', 'Scoundrel')
  #write_output('Wizard', 'Elementalist')
  #write_output('Priest', 'Light')

  #write_class_section_file(df_class, dir_output)
  
  succeed() # Finished
})

