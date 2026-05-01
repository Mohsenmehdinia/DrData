test_that("ml_prepare_data returns correct structure", {
  prep <- ml_prepare_data(mtcars, target = "mpg")
  expect_type(prep, "list")
  expect_named(prep, c("data", "target", "features"))
  expect_equal(prep$target, "mpg")
  expect_false("mpg" %in% prep$features)
  expect_true(all(prep$features %in% names(mtcars)))
})

test_that("ml_prepare_data drops incomplete rows", {
  df <- mtcars
  df$mpg[1:3] <- NA
  prep <- ml_prepare_data(df, "mpg")
  expect_equal(nrow(prep$data), nrow(mtcars) - 3)
})

test_that("ml_prepare_data rejects missing target", {
  expect_error(ml_prepare_data(mtcars, target = "nonexistent"))
})

test_that("ml_prepare_data rejects missing features", {
  expect_error(ml_prepare_data(mtcars, target = "mpg",
                               features = c("cyl", "bad_col")))
})
