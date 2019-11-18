context("Convert monster data from .csv to html")


test_that("get_htm_monster_race() works", {
  file_css <- system.file("SFRPG-monsters.css", package='sfrpg',
                          mustWork=TRUE)
  css <- readChar(file_css, file.info(file_css)$size)
  htm_monster_race <- get_htm_monster_race() %>% get_htm_file(css=css)
  verify_output(
    test_path('output','test_get_htm_monster_race.txt'),
    print(htm_monster_race)
  )
  htm_monster_race %>% readr::write_file(test_path('output','test_get_htm_monster_race.html'))
})

test_that("get_htm_monster_class() works", {
  file_css <- system.file("SFRPG-monsters.css", package='sfrpg',
                          mustWork=TRUE)
  css <- readChar(file_css, file.info(file_css)$size)
  htm_monster_class <- get_htm_monster_class() %>% get_htm_file(css=css)
  verify_output(
    test_path('output','test_get_htm_monster_class.txt'),
    print(htm_monster_class)
  )
  htm_monster_class %>% readr::write_file(test_path('output','test_get_htm_monster_class.html'))

})
