
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
