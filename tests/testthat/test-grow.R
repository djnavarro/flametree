
test_that("invalid seeds are forbidden", {

  # individual error checks (check that the correct error is thrown)
  expect_error(flametree_grow(seed = NULL), "must not be null")
  expect_error(flametree_grow(seed = NA), "must not contain missing values")
  expect_error(flametree_grow(seed = .1234), "must be integer valued")
  expect_error(flametree_grow(seed = 1:3), "must have length")
  expect_error(flametree_grow(seed = numeric(0L)), "must have length")
  expect_error(flametree_grow(seed = "abc"), "must be numeric")
  expect_error(flametree_grow(seed = TRUE), "must be numeric")
  expect_error(flametree_grow(seed = list(1)), "must be numeric")

  # combination error checks (don't care which error is thrown, just
  # checking that weird edge cases don't escape the checks)
  expect_error(flametree_grow(seed = c(1, NA)))
  expect_error(flametree_grow(seed = list(1, NA)))

  # edge cases that should pass without messages or warnings
  expect_silent(flametree_grow(seed = 0)) # zero ok
  expect_silent(flametree_grow(seed = -1)) # negative numbers okay
})


test_that("invalid times are forbidden", {

  expect_error(flametree_grow(time = 0))
  expect_error(flametree_grow(time = -1))
  expect_error(flametree_grow(time = .3))
  expect_error(flametree_grow(time = 1:4))
  expect_error(flametree_grow(time = NA_integer_))
  expect_error(flametree_grow(time = NaN))
  expect_error(flametree_grow(time = Inf))
  expect_error(flametree_grow(time = "abc"))
  expect_error(flametree_grow(time = NULL))
  expect_error(flametree_grow(time = TRUE))
  expect_error(flametree_grow(time = list(2)))

  expect_silent(flametree_grow(time = 3))

})


test_that("invalid scales are forbidden",{

  expect_error(flametree_grow(scale = "abc"))
  expect_error(flametree_grow(scale = TRUE))
  expect_error(flametree_grow(scale = list()))
  expect_error(flametree_grow(scale = NULL))
  expect_error(flametree_grow(scale = NA))
  expect_error(flametree_grow(scale = -.123))
  expect_error(flametree_grow(scale = numeric(0)))
  expect_error(flametree_grow(scale = character(0)))
  expect_error(flametree_grow(scale = c(.8, -.123, .1)))
  expect_error(flametree_grow(scale = c(.8, NA, .1)))
  expect_error(flametree_grow(scale = 0))

  expect_silent(flametree_grow(scale = c(.8, .9)))
  expect_silent(flametree_grow(scale = c(.8, .9, 1.1)))

})

test_that("invalid angles are forbidden",{

  expect_error(flametree_grow(angle = "abc"))
  expect_error(flametree_grow(angle = TRUE))
  expect_error(flametree_grow(angle = list()))
  expect_error(flametree_grow(angle = NULL))
  expect_error(flametree_grow(angle = NA))
  expect_error(flametree_grow(angle = numeric(0)))
  expect_error(flametree_grow(angle = character(0)))
  expect_error(flametree_grow(angle = c(.8, NA, 10)))
  expect_error(flametree_grow(angle = -12.3))

  expect_silent(flametree_grow(angle = c(0, -12.3)))
  expect_silent(flametree_grow(angle = c(-700, 1000)))

})


test_that("invalid splits are forbidden", {

  expect_error(flametree_grow(split = 0))
  expect_error(flametree_grow(split = -1))
  expect_error(flametree_grow(split = .3))
  expect_error(flametree_grow(split = 1:4))
  expect_error(flametree_grow(split = NA_integer_))
  expect_error(flametree_grow(split = NaN))
  expect_error(flametree_grow(split = Inf))
  expect_error(flametree_grow(split = "abc"))
  expect_error(flametree_grow(split = NULL))
  expect_error(flametree_grow(split = TRUE))
  expect_error(flametree_grow(split = list(2)))

  expect_silent(flametree_grow(split = 3))

})


test_that("flametree data has correct columns", {

  dat <- flametree_grow()

  expect_s3_class(dat, "tbl")
  expect_named(
    object = dat,
    expected = c(
      "coord_x", "coord_y", "id_tree", "id_time", "id_path", "id_leaf",
      "id_pathtree", "id_step", "seg_deg", "seg_len", "seg_col", "seg_wid"
    )
  )

  expect_type(dat$coord_x, "double")
  expect_type(dat$coord_y, "double")
  expect_type(dat$seg_deg, "double")
  expect_type(dat$seg_len, "double")
  expect_type(dat$seg_col, "double")
  expect_type(dat$seg_wid, "double")
  expect_type(dat$id_time, "integer")
  expect_type(dat$id_path, "integer")
  expect_type(dat$id_step, "integer")
  expect_type(dat$id_leaf, "logical")
  expect_type(dat$id_tree, "integer")
  expect_type(dat$id_pathtree, "character")

})

test_that("flametree edges are well defined", {

  dat <- flametree_grow()

  # three rows per edge
  expect_equal(nrow(dat) %% 3, 0)
  expect_equal(nrow(dat), length(unique(dat$id_path)) * 3)

  # same number of rows at each step
  expect_equal(sum(dat$id_step == 0), sum(dat$id_step == 1))
  expect_equal(sum(dat$id_step == 0), sum(dat$id_step == 2))

  # id_path and id_step uniquely define the row
  expect_true(all(table(paste(dat$id_path, dat$id_step)) == 1))

})


