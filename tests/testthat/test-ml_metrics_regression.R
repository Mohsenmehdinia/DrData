test_that("ml_metrics_regression returns correct structure", {
  m <- ml_metrics_regression(1:5, 1:5)
  expect_s3_class(m, "data.frame")
  expect_named(m, c("RMSE", "MAE", "R2"))
  expect_equal(m$RMSE, 0)
  expect_equal(m$MAE,  0)
  expect_equal(m$R2,   1)
})

test_that("ml_metrics_regression detects bad predictions", {
  y_true <- c(1, 2, 3, 4, 5)
  y_pred <- c(2, 3, 4, 5, 6)
  m <- ml_metrics_regression(y_true, y_pred)
  expect_equal(m$RMSE, 1)
  expect_equal(m$MAE,  1)
  expect_true(m$R2 < 1)
})

test_that("ml_metrics_regression handles NA with warning", {
  expect_warning(
    ml_metrics_regression(c(1, NA, 3), c(1, 2, NA))
  )
})

test_that("ml_metrics_regression rejects mismatched lengths", {
  expect_error(ml_metrics_regression(1:3, 1:4))
})
