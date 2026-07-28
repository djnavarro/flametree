
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


test_that("invalid plot objects are forbidden", {

  tempimage <- paste0(tempfile(), ".png")

  expect_error(flametree_save(NULL, tempimage), "must not be null")
  expect_error(flametree_save("not a plot", tempimage), "must be a ggplot object")
  expect_error(flametree_save(123, tempimage), "must be a ggplot object")
  expect_error(flametree_save(data.frame(x = 1), tempimage), "must be a ggplot object")
  expect_error(flametree_save(list(a = 1), tempimage), "must be a ggplot object")

  expect_false(file.exists(tempimage))

})
