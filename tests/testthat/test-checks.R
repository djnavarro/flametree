# included mostly as a way to document precisely what behaviour
# the minimal checks actually enforce: in a few cases they feel
# counterintuitive and I like being reminded that the edge cases
# exist

with_attributes <- function(x, value = list(blah = "blah")) {
  attributes(x) <- value
  return(x)
}

test_that("check_not_null works", {
  expect_error(check_not_null(NULL, "name"), "must not be null")

  # cases that should pass
  expect_silent(check_not_null(NA, "name"))
  expect_silent(check_not_null(NaN, "name"))
  expect_silent(check_not_null(Inf, "name"))
  expect_silent(check_not_null(FALSE, "name"))
  expect_silent(check_not_null(1, "name"))
  expect_silent(check_not_null(1:3, "name"))
  expect_silent(check_not_null(numeric(0L), "name"))
  expect_silent(check_not_null("a", "name"))
  expect_silent(check_not_null(list(NULL), "name"))
  expect_silent(check_not_null(complex(real = 1, imaginary = 1), "name"))
})

test_that("check_not_na works", {

  check_not_na_pattern <- "must not contain missing values"

  # cases that should fail
  expect_error(check_not_na(NA, "name"), check_not_na_pattern)
  expect_error(check_not_na(NA_character_, "name"), check_not_na_pattern)
  expect_error(check_not_na(NA_complex_, "name"), check_not_na_pattern)
  expect_error(check_not_na(NA_integer_, "name"), check_not_na_pattern)
  expect_error(check_not_na(NA_real_, "name"), check_not_na_pattern)
  expect_error(check_not_na(c(1, 2, NA), "name"), check_not_na_pattern)
  expect_error(check_not_na(c(TRUE, NA), "name"), check_not_na_pattern)
  expect_error(check_not_na(c("abc", NA), "name"), check_not_na_pattern)
  expect_error(check_not_na(with_attributes(NA), "name"), check_not_na_pattern)
  expect_error(check_not_na(c("blah" = NA), "name"), check_not_na_pattern)
  expect_error(check_not_na(complex(real = 1, imaginary = NA), "name"), check_not_na_pattern)

  # cases that should pass
  expect_silent(check_not_na(NULL, "name"))
  expect_silent(check_not_na(Inf, "name"))
  expect_silent(check_not_na(FALSE, "name"))
  expect_silent(check_not_na(1, "name"))
  expect_silent(check_not_na(1:3, "name"))
  expect_silent(check_not_na(numeric(0L), "name"))
  expect_silent(check_not_na("a", "name"))
  expect_silent(check_not_na(complex(real = 1, imaginary = 1), "name"))

  # the weird cases, included as a reminder to the unwary...
  expect_error(check_not_na(NaN, "name"), check_not_na_pattern)
  expect_error(check_not_na(list(NA), "name"), check_not_na_pattern)
  expect_error(check_not_na(list("blah" = NA), "name"), check_not_na_pattern)
  expect_silent(check_not_na(list(c(1, NA)), "name"))
})

test_that("check_numeric works", {

  check_numeric_pattern <- "must be numeric"

  # cases that should fail
  expect_error(check_numeric("abc", "name"), check_numeric_pattern)
  expect_error(check_numeric(TRUE, "name"), check_numeric_pattern)
  expect_error(check_numeric(NA, "name"), check_numeric_pattern)
  expect_error(check_numeric(NA_complex_, "name"), check_numeric_pattern)
  expect_error(check_numeric(complex(real = 1, imaginary = NA), "name"), check_numeric_pattern)
  expect_error(check_numeric(list(123), "name"), check_numeric_pattern)

  # cases that should pass
  expect_silent(check_numeric(123, "name"))
  expect_silent(check_numeric(12.3, "name"))
  expect_silent(check_numeric(-124, "name"))
  expect_silent(check_numeric(numeric(0), "name"))
  expect_silent(check_numeric(integer(0), "name"))
  expect_silent(check_numeric(1:3, "name"))
  expect_silent(check_numeric(numeric(0L), "name"))
  expect_silent(check_numeric(NA_real_, "name"))
  expect_silent(check_numeric(NA_integer_, "name"))
  expect_silent(check_numeric(Inf, "name"))
  expect_silent(check_numeric(NaN, "name"))
  expect_silent(check_numeric(with_attributes(1), "name"))

})


test_that("check_soft_integer works", {

  check_soft_integer_pattern <- "must be integer valued"

  # cases that should fail by virtue of being non-numeric (don't check
  # error pattern because I don't care which function throws the error)
  expect_error(check_soft_integer("abc", "name"))
  expect_error(check_soft_integer(TRUE, "name"))
  expect_error(check_soft_integer(NA, "name"))
  expect_error(check_soft_integer(NA_complex_, "name"))
  expect_error(check_soft_integer(complex(real = 1, imaginary = NA), "name"))
  expect_error(check_soft_integer(list(123), "name"))

  # numeric non-integers
  expect_error(check_soft_integer(12.3, "name"), check_soft_integer_pattern)
  expect_error(check_soft_integer(NA_real_, "name"), check_soft_integer_pattern)
  expect_error(check_soft_integer(NaN, "name"), check_soft_integer_pattern)
  expect_error(check_soft_integer(Inf, "name"), check_soft_integer_pattern)

  # cases that should pass
  expect_silent(check_soft_integer(123, "name"))
  expect_silent(check_soft_integer(-12, "name"))
  expect_silent(check_soft_integer(0, "name"))
  expect_silent(check_soft_integer(c(1L, NA), "name"))
  expect_silent(check_soft_integer(NA_integer_, "name"))

})

