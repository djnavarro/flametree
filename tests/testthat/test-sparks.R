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



