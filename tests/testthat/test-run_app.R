test_that("run_app function is exported and callable", {
  expect_true(is.function(DrData::run_app))
})

test_that("app directory exists in package", {
  app_dir <- system.file("app", package = "DrData")
  expect_true(nchar(app_dir) > 0)
  expect_true(file.exists(app_dir))
})
