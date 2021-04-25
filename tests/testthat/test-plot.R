
test_that("plot returns a ggplot object", {

  dat <- flametree_grow(time = 5)

  expect_s3_class(flametree_plot(dat, style = "plain"), "gg")
  expect_s3_class(flametree_plot(dat, style = "minimal"), "gg")
  expect_s3_class(flametree_plot(dat, style = "themegray"), "gg")
  expect_s3_class(flametree_plot(dat, style = "voronoi"), "gg")
  expect_s3_class(flametree_plot(dat, style = "wisp"), "gg")
  expect_s3_class(flametree_plot(dat, style = "nativeflora"), "gg")

})

test_that("invalid plot inputs throw errors", {

  dat <- flametree_grow(time = 5)

  expect_error(flametree_plot(dat, style = 124))
  expect_error(flametree_plot(dat, style = TRUE))
  expect_error(flametree_plot(dat, style = list("plain")))
  expect_error(flametree_plot(dat, style = NULL))
  expect_error(flametree_plot(dat, style = function(x){x}))
  expect_error(flametree_plot(dat, style = NA))
  expect_error(flametree_plot(dat, style = c("plain", "minimal")))

  expect_error(flametree_plot(dat, palette = 124))
  expect_error(flametree_plot(dat, palette = TRUE))
  expect_error(flametree_plot(dat, palette = list("plain")))
  expect_error(flametree_plot(dat, palette = NULL))
  expect_error(flametree_plot(dat, palette = function(x){x}))
  expect_error(flametree_plot(dat, palette = NA))

  expect_error(flametree_plot(dat, background = 124))
  expect_error(flametree_plot(dat, background = TRUE))
  expect_error(flametree_plot(dat, background = list("plain")))
  expect_error(flametree_plot(dat, background = NULL))
  expect_error(flametree_plot(dat, background = function(x){x}))
  expect_error(flametree_plot(dat, background = NA))
  expect_error(flametree_plot(dat, background = c("plain", "minimal")))

  expect_error(flametree_plot(dat, data = 124))
  expect_error(flametree_plot(dat, data = TRUE))
  expect_error(flametree_plot(dat, data = list("plain")))
  expect_error(flametree_plot(dat, data = NULL))
  expect_error(flametree_plot(dat, data = function(x){x}))
  expect_error(flametree_plot(dat, data = NA))
  expect_error(flametree_plot(dat, data = c("plain", "minimal")))

})


test_that("invalid plot style names throw error", {

  dat <- flametree_grow(time = 5)
  style_error_msg <- 'must be "plain", "minimal", "themegray", "voronoi", "wisp", or "nativeflora"'

  expect_error(flametree_plot(dat, style = "abc"), style_error_msg)

})
