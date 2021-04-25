
test_that("plot save works", {

  tempimage <- paste0(tempfile(), ".png")
  dat <- flametree_grow()
  pic <- flametree_plot(dat)

  expect_false(file.exists(tempimage))
  flametree_save(pic, tempimage)
  expect_true(file.exists(tempimage))
  file.remove(tempimage)
  expect_false(file.exists(tempimage))

})
