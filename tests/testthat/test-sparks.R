test_that("spark function factories return functions", {
  expect_type(spark_decay(), "closure")
  expect_type(spark_linear(), "closure")
  expect_type(spark_random(), "closure")
  expect_type(spark_nothing(), "closure")
})

