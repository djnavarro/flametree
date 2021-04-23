
test_that("plot returns a ggplot object", {

  dat <- flametree_grow(time = 5)
  pic <- flametree_plot(dat)

  expect_s3_class(pic, "gg")
})

