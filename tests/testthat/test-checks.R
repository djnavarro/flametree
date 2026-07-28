# included mostly as a way to document precisely what behaviour
# the minimal checks actually enforce: in a few cases they feel
# counterintuitive and I like being reminded that the edge cases
# exist

with_attributes <- function(x, value = list(blah = "blah")) {
  attributes(x) <- value
  return(x)
}

test_that("ft__check_not_null works", {
  expect_error(ft__check_not_null(NULL, "name"), "must not be null")

  # cases that should pass
  expect_silent(ft__check_not_null(NA, "name"))
  expect_silent(ft__check_not_null(NaN, "name"))
  expect_silent(ft__check_not_null(Inf, "name"))
  expect_silent(ft__check_not_null(FALSE, "name"))
  expect_silent(ft__check_not_null(1, "name"))
  expect_silent(ft__check_not_null(1:3, "name"))
  expect_silent(ft__check_not_null(numeric(0L), "name"))
  expect_silent(ft__check_not_null("a", "name"))
  expect_silent(ft__check_not_null(list(NULL), "name"))
  expect_silent(ft__check_not_null(complex(real = 1, imaginary = 1), "name"))
})

test_that("ft__check_not_na works", {

  ft__check_not_na_pattern <- "must not contain missing values"

  # cases that should fail
  expect_error(ft__check_not_na(NA, "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(NA_character_, "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(NA_complex_, "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(NA_integer_, "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(NA_real_, "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(c(1, 2, NA), "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(c(TRUE, NA), "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(c("abc", NA), "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(with_attributes(NA), "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(c("blah" = NA), "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(complex(real = 1, imaginary = NA), "name"), ft__check_not_na_pattern)

  # cases that should pass
  expect_silent(ft__check_not_na(NULL, "name"))
  expect_silent(ft__check_not_na(Inf, "name"))
  expect_silent(ft__check_not_na(FALSE, "name"))
  expect_silent(ft__check_not_na(1, "name"))
  expect_silent(ft__check_not_na(1:3, "name"))
  expect_silent(ft__check_not_na(numeric(0L), "name"))
  expect_silent(ft__check_not_na("a", "name"))
  expect_silent(ft__check_not_na(complex(real = 1, imaginary = 1), "name"))

  # the weird cases, included as a reminder to the unwary...
  expect_error(ft__check_not_na(NaN, "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(list(NA), "name"), ft__check_not_na_pattern)
  expect_error(ft__check_not_na(list("blah" = NA), "name"), ft__check_not_na_pattern)
  expect_silent(ft__check_not_na(list(c(1, NA)), "name"))
})

test_that("ft__check_numeric works", {

  ft__check_numeric_pattern <- "must be numeric"

  # cases that should fail
  expect_error(ft__check_numeric("abc", "name"), ft__check_numeric_pattern)
  expect_error(ft__check_numeric(TRUE, "name"), ft__check_numeric_pattern)
  expect_error(ft__check_numeric(NA, "name"), ft__check_numeric_pattern)
  expect_error(ft__check_numeric(NA_complex_, "name"), ft__check_numeric_pattern)
  expect_error(ft__check_numeric(complex(real = 1, imaginary = NA), "name"), ft__check_numeric_pattern)
  expect_error(ft__check_numeric(list(123), "name"), ft__check_numeric_pattern)

  # cases that should pass
  expect_silent(ft__check_numeric(123, "name"))
  expect_silent(ft__check_numeric(12.3, "name"))
  expect_silent(ft__check_numeric(-124, "name"))
  expect_silent(ft__check_numeric(numeric(0), "name"))
  expect_silent(ft__check_numeric(integer(0), "name"))
  expect_silent(ft__check_numeric(1:3, "name"))
  expect_silent(ft__check_numeric(numeric(0L), "name"))
  expect_silent(ft__check_numeric(NA_real_, "name"))
  expect_silent(ft__check_numeric(NA_integer_, "name"))
  expect_silent(ft__check_numeric(Inf, "name"))
  expect_silent(ft__check_numeric(NaN, "name"))
  expect_silent(ft__check_numeric(with_attributes(1), "name"))

})


test_that("ft__check_colour works", {

  ft__check_colour_pattern <- "must contain valid colours"

  # cases that should fail
  expect_error(ft__check_colour("##f6b6a6", "name"), ft__check_colour_pattern)
  expect_error(ft__check_colour("not_a_colour", "name"), ft__check_colour_pattern)
  expect_error(ft__check_colour(c("red", "not_a_colour"), "name"), ft__check_colour_pattern)
  expect_error(ft__check_colour("#12345", "name"), ft__check_colour_pattern)
  expect_error(ft__check_colour(123, "name")) # fails ft__check_character first

  # cases that should pass
  expect_silent(ft__check_colour("red", "name"))
  expect_silent(ft__check_colour("black", "name"))
  expect_silent(ft__check_colour("#f6b6a6", "name"))
  expect_silent(ft__check_colour("#f6b6a6ff", "name"))
  expect_silent(ft__check_colour(c("red", "#000000", "blue"), "name"))

})


test_that("ft__estimate_rows works", {

  # 1 tree, time = 0 (just the sapling): 3 rows regardless of split
  expect_equal(ft__estimate_rows(time = 0, split = 2, trees = 1), 3)

  # defaults-like case: time = 6, split = 2, trees = 1
  # layers 0:6 of split 2 sum to 2^7 - 1 = 127, times 3 = 381
  expect_equal(ft__estimate_rows(time = 6, split = 2, trees = 1), 381)

  # scales linearly with trees
  expect_equal(
    ft__estimate_rows(time = 6, split = 2, trees = 5),
    5 * ft__estimate_rows(time = 6, split = 2, trees = 1)
  )

})

test_that("ft__check_growth_size works", {

  small <- list(time = 6, split = 2, trees = 1)
  large <- list(time = 20, split = 6, trees = 10)

  expect_silent(ft__check_growth_size(small))
  expect_warning(ft__check_growth_size(large), "will generate approximately")

  # respects a custom row_limit
  expect_warning(ft__check_growth_size(small, row_limit = 10), "will generate approximately")

})


test_that("ft__check_soft_integer works", {

  ft__check_soft_integer_pattern <- "must be integer valued"

  # cases that should fail by virtue of being non-numeric (don't check
  # error pattern because I don't care which function throws the error)
  expect_error(ft__check_soft_integer("abc", "name"))
  expect_error(ft__check_soft_integer(TRUE, "name"))
  expect_error(ft__check_soft_integer(NA, "name"))
  expect_error(ft__check_soft_integer(NA_complex_, "name"))
  expect_error(ft__check_soft_integer(complex(real = 1, imaginary = NA), "name"))
  expect_error(ft__check_soft_integer(list(123), "name"))

  # numeric non-integers
  expect_error(ft__check_soft_integer(12.3, "name"), ft__check_soft_integer_pattern)
  expect_error(ft__check_soft_integer(NA_real_, "name"), ft__check_soft_integer_pattern)
  expect_error(ft__check_soft_integer(NaN, "name"), ft__check_soft_integer_pattern)
  expect_error(ft__check_soft_integer(Inf, "name"), ft__check_soft_integer_pattern)

  # cases that should pass
  expect_silent(ft__check_soft_integer(123, "name"))
  expect_silent(ft__check_soft_integer(-12, "name"))
  expect_silent(ft__check_soft_integer(0, "name"))
  expect_silent(ft__check_soft_integer(c(1L, NA), "name"))
  expect_silent(ft__check_soft_integer(NA_integer_, "name"))

})

