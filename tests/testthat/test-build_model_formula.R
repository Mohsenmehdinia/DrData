test_that("build_model_formula basic formula is correct", {
  f <- build_model_formula("mpg", c("cyl", "hp", "wt"))
  expect_s3_class(f, "formula")
  expect_equal(deparse(f), "mpg ~ cyl + hp + wt")
})

test_that("build_model_formula adds interactions", {
  f <- build_model_formula("mpg", c("cyl", "hp", "wt"),
                           use_interactions = TRUE,
                           interaction_vars = c("cyl", "hp"))
  expect_true(grepl("cyl:hp", deparse(f)))
})

test_that("build_model_formula ignores interactions when < 2 vars", {
  f <- build_model_formula("mpg", c("cyl", "hp"),
                           use_interactions = TRUE,
                           interaction_vars = c("cyl"))
  expect_false(grepl(":", deparse(f)))
})
