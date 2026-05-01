test_that("ml_split returns correct proportions", {
  df     <- data.frame(x = 1:100, y = rnorm(100))
  splits <- ml_split(df, train_ratio = 0.8, seed = 42)
  expect_type(splits, "list")
  expect_named(splits, c("train", "test"))
  expect_equal(nrow(splits$train) + nrow(splits$test), nrow(df))
  expect_true(nrow(splits$train) >= 78 && nrow(splits$train) <= 82)
})

test_that("ml_split is reproducible with same seed", {
  df <- mtcars
  s1 <- ml_split(df, seed = 1)
  s2 <- ml_split(df, seed = 1)
  expect_identical(s1$train, s2$train)
})

test_that("ml_split rejects invalid inputs", {
  expect_error(ml_split(list(a=1), 0.8))
  expect_error(ml_split(mtcars, 1.5))
  expect_error(ml_split(mtcars, 0))
})
