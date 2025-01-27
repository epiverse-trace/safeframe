test_that("tests for validate_tags", {
  # test errors
  msg <- "Must inherit from class 'safeframe', but has class 'data.frame'."
  expect_error(validate_tags(cars), msg)

  x <- make_safeframe(cars)
  msg <- "`x` has no tags"
  expect_error(validate_tags(x), msg)

  # functionalities
  x <- make_safeframe(cars)
  expect_error(validate_tags(x))

  x <- set_tags(x, dist = "Distance in miles", speed = "Miles per hour")
  expect_identical(x, validate_tags(x))
})
