test_that("spark function factories return closures", {
  expect_type(spark_decay(), "closure")
  expect_type(spark_linear(), "closure")
  expect_type(spark_random(), "closure")
  expect_type(spark_nothing(), "closure")
})

test_that("spark functions have correct arguments", {
  spark_args <- c("coord_x", "coord_y", "id_tree", "id_time")
  expect_equal(methods::formalArgs(spark_decay()), spark_args)
  expect_equal(methods::formalArgs(spark_linear()), spark_args)
  expect_equal(methods::formalArgs(spark_random()), spark_args)
  expect_equal(methods::formalArgs(spark_nothing()), spark_args)
})

test_that("spark functions return numeric", {

  expect_type((spark_decay())(1, 1, 1, 1), "double")
  expect_type((spark_linear())(1, 1, 1, 1), "double")
  expect_type((spark_random())(1, 1, 1, 1), "double")
  expect_type((spark_nothing())(1, 1, 1, 1), "double")

})


test_that("spark functions handle vector input", {

  coord_x <- 1:6
  coord_y <- 1:6
  id_tree <- c(1,1,1,2,2,2)
  id_time <- c(1,2,3,1,2,3)

  expect_length((spark_decay())(coord_x, coord_y, id_tree, id_time), 6)
  expect_length((spark_linear())(coord_x, coord_y, id_tree, id_time), 6)
  expect_length((spark_random())(coord_x, coord_y, id_tree, id_time), 6)
  expect_length((spark_nothing())(coord_x, coord_y, id_tree, id_time), 6)

})


test_that("spark factories verify numeric input", {

  error_pattern <- function(name) {
    paste0("`", name, "` must be numeric")
  }

  expect_error(spark_decay(x = "abc"), error_pattern("x"))
  expect_error(spark_decay(y = "abc"), error_pattern("y"))
  expect_error(spark_decay(tree = "abc"), error_pattern("tree"))
  expect_error(spark_decay(time = "abc"), error_pattern("time"))
  expect_error(spark_decay(multiplier = "abc"), error_pattern("multiplier"))
  expect_error(spark_decay(constant = "abc"), error_pattern("constant"))

  expect_error(spark_linear(x = "abc"), error_pattern("x"))
  expect_error(spark_linear(y = "abc"), error_pattern("y"))
  expect_error(spark_linear(tree = "abc"), error_pattern("tree"))
  expect_error(spark_linear(time = "abc"), error_pattern("time"))
  expect_error(spark_linear(constant = "abc"), error_pattern("constant"))

  expect_error(spark_random(multiplier = "abc"), error_pattern("multiplier"))
  expect_error(spark_random(constant = "abc"), error_pattern("constant"))

})


test_that("spark factories verify length 1 input", {

  error_pattern <- function(name) {
    paste0("`", name, "` must have length 1")
  }

  expect_error(spark_decay(x = 1:3), error_pattern("x"))
  expect_error(spark_decay(y = 1:3), error_pattern("y"))
  expect_error(spark_decay(tree = 1:3), error_pattern("tree"))
  expect_error(spark_decay(time = 1:3), error_pattern("time"))
  expect_error(spark_decay(multiplier = 1:3), error_pattern("multiplier"))
  expect_error(spark_decay(constant = 1:3), error_pattern("constant"))

  expect_error(spark_linear(x = 1:3), error_pattern("x"))
  expect_error(spark_linear(y = 1:3), error_pattern("y"))
  expect_error(spark_linear(tree = 1:3), error_pattern("tree"))
  expect_error(spark_linear(time = 1:3), error_pattern("time"))
  expect_error(spark_linear(constant = 1:3), error_pattern("constant"))

  expect_error(spark_random(multiplier = 1:3), error_pattern("multiplier"))
  expect_error(spark_random(constant = 1:3), error_pattern("constant"))

})




test_that("spark factories verify non-missing input", {

  error_pattern <- function(name) {
    paste0("`", name, "` must not contain missing values")
  }

  expect_error(spark_decay(x = NA_real_), error_pattern("x"))
  expect_error(spark_decay(y = NA_real_), error_pattern("y"))
  expect_error(spark_decay(tree = NA_real_), error_pattern("tree"))
  expect_error(spark_decay(time = NA_real_), error_pattern("time"))
  expect_error(spark_decay(multiplier = NA_real_), error_pattern("multiplier"))
  expect_error(spark_decay(constant = NA_real_), error_pattern("constant"))

  expect_error(spark_linear(x = NA_real_), error_pattern("x"))
  expect_error(spark_linear(y = NA_real_), error_pattern("y"))
  expect_error(spark_linear(tree = NA_real_), error_pattern("tree"))
  expect_error(spark_linear(time = NA_real_), error_pattern("time"))
  expect_error(spark_linear(constant = NA_real_), error_pattern("constant"))

  expect_error(spark_random(multiplier = NA_real_), error_pattern("multiplier"))
  expect_error(spark_random(constant = NA_real_), error_pattern("constant"))

})





