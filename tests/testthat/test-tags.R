test_that("tests for tags", {
  # Check error messages
  x <- make_safeframe(cars, speed = "Miles per hour")

  # Check functionality
  expect_identical(tags(x), list(speed = "Miles per hour"))
  expect_identical(tags(x, show_null = TRUE), list(
    speed = "Miles per hour",
    dist = NULL
  ))

  # tags() returns an empty named list, which we cannot compare to list()
  # directly.
  expect_identical(length(tags(make_safeframe(cars))), length(list()))
  expect_identical(tags(make_safeframe(cars), TRUE), list(
    speed = NULL,
    dist = NULL
  ))
})
