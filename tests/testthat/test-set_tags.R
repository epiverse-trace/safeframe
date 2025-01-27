test_that("tests for set_tags()", {
  x <- make_safeframe(cars, dist = "Distance")

  # Check whether error messages are the same as before
  # Uses snapshot to prevent formatting issues in validating the error message
  expect_snapshot(set_tags(cars), error = TRUE)
  expect_snapshot(set_tags(x, toto = "speed"), error = TRUE)

  # Check functionality
  expect_identical(x, set_tags(x))
  x <- set_tags(x, speed = "Miles per hour")
  expect_identical(tags(x)$speed, "Miles per hour")
  expect_identical(tags(x)$dist, "Distance")

  x <- set_tags(x, speed = "Km per hour", dist = "Kilometre distance")
  y <- set_tags(x, !!!list(speed = "Km per hour", dist = "Kilometre distance"))
  expect_identical(x, y)
})
