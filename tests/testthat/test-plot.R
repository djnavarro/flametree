
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

test_that("non-flametree data frames throw plot error", {

  dat <- flametree_grow(time = 5)

  new_dat <- dat[,1:4]
  expect_error(flametree_plot(new_dat), "must have length 12")

  new_dat <- dat
  names(new_dat)[1] <- "blah"
  expect_error(flametree_plot(new_dat), "unexpected column names")

  new_dat <- dat
  new_dat$coord_x <- as.character(new_dat$coord_x)
  expect_error(flametree_plot(new_dat), "must be numeric")

  new_dat <- dat
  new_dat$id_leaf <- as.character(new_dat$id_leaf)
  expect_error(flametree_plot(new_dat), "must be logical")

})


test_that("invalid plot style names throw error", {

  dat <- flametree_grow(time = 5)
  style_error_msg <- 'must be "plain", "minimal", "themegray", "voronoi", "wisp", or "nativeflora"'

  expect_error(flametree_plot(dat, style = "abc"), style_error_msg)

})

# don't waste CRAN compute time
skip_on_cran()

test_that("grow & plot works for typical inputs", {

  times <- 2:3
  seeds <- 1:2
  scales <- list(c(.5, .8), c(0, .5, 1))
  angles <- list( c(0, 1000))
  trees <- 1:3
  splits <- 2:3
  styles <- c("plain", "minimal", "themegray",
              "voronoi", "wisp", "nativeflora")

  for(tm in times) {
    for(sd in seeds) {
      for(sc in scales) {
        for(tr in trees) {
          for(sp in splits) {
            for(an in angles) {

              dat <- flametree_grow(seed = sd, time = tm, scale = sc, angle = an,
                                    trees = tr, split = sp)

              expect_s3_class(dat, "tbl_df")
              for(st in styles) {

                pic <- flametree_plot(dat, style = st)
                expect_s3_class(pic, "ggplot")

              }
            }
          }
        }
      }
    }
  }


})

