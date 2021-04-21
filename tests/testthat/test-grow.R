
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
  expect_error(flametree_grow(scale = c(.8, -.123)))
  expect_error(flametree_grow(scale = c(.8, NA)))

  expect_silent(flametree_grow(scale = 0))
  expect_silent(flametree_grow(scale = .8))
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
  expect_error(flametree_grow(angle = c(.8, NA)))

  expect_silent(flametree_grow(angle = -12.3))
  expect_silent(flametree_grow(angle = c(0, -12.3)))
  expect_silent(flametree_grow(angle = 0))
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


test_that("invalid prunes are forbidden", {

  expect_error(flametree_grow(prune = -1))
  expect_error(flametree_grow(prune = 3))
  expect_error(flametree_grow(prune = NA_real_))
  expect_error(flametree_grow(prune = NaN))
  expect_error(flametree_grow(prune = Inf))
  expect_error(flametree_grow(prune = "abc"))
  expect_error(flametree_grow(prune = NULL))
  expect_error(flametree_grow(prune = TRUE))
  expect_error(flametree_grow(prune = list(2)))
  expect_error(flametree_grow(prune = c(.1, .2)))


  expect_silent(flametree_grow(prune = .3))
  expect_silent(flametree_grow(prune = 0))
  expect_silent(flametree_grow(prune = 1))

})



