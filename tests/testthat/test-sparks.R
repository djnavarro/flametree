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
