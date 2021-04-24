
test_that("plot returns a ggplot object", {

  dat <- flametree_grow(time = 5)

  expect_s3_class(flametree_plot(dat, style = "plain"), "gg")
  expect_s3_class(flametree_plot(dat, style = "minimal"), "gg")
  expect_s3_class(flametree_plot(dat, style = "themegray"), "gg")
  expect_s3_class(flametree_plot(dat, style = "voronoi"), "gg")
  expect_s3_class(flametree_plot(dat, style = "wisp"), "gg")
  expect_s3_class(flametree_plot(dat, style = "nativeflora"), "gg")
})

