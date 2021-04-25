
test_that("plot returns a ggplot object", {

  dat <- flametree_grow(time = 5)

  expect_s3_class(flametree_plot(dat, style = "plain"), "gg")
  expect_s3_class(flametree_plot(dat, style = "minimal"), "gg")
  expect_s3_class(flametree_plot(dat, style = "themegray"), "gg")
  expect_s3_class(flametree_plot(dat, style = "voronoi"), "gg")
  expect_s3_class(flametree_plot(dat, style = "wisp"), "gg")
  expect_s3_class(flametree_plot(dat, style = "nativeflora"), "gg")

})


test_that("invalid plot styles throw error", {

  dat <- flametree_grow(time = 5)
  style_error_msg <- 'must be "plain", "minimal", "themegray", "voronoi", "wisp", or "nativeflora"'

  expect_error(flametree_plot(dat, style = "abc"), style_error_msg)
  expect_error(flametree_plot(dat, style = 124), style_error_msg)
  expect_error(flametree_plot(dat, style = TRUE), style_error_msg)
  expect_error(flametree_plot(dat, style = list("plain")), style_error_msg)
  expect_error(flametree_plot(dat, style = NULL), style_error_msg)
  expect_error(flametree_plot(dat, style = function(x){x}), style_error_msg)

  # cases to catch!!!
  #expect_error(flametree_plot(dat, style = NA), style_error_msg)
  #expect_error(flametree_plot(dat, style = c("plain", "minimal")), style_error_msg)

})
