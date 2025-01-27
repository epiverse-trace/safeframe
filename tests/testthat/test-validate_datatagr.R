test_that("tests for validate_safeframe", {
  # errors
  msg <- "Must inherit from class 'safeframe', but has class 'NULL'."
  expect_error(validate_safeframe(NULL), msg)

  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  msg <- "Assertion on 'types' failed: Must have length >= 1, but has length 0."
  expect_error(validate_safeframe(x), msg)
  expect_identical(x, validate_safeframe(x,
    speed = "numeric",
    dist = "numeric"
  ))

  x <- make_safeframe(cars, speed = "Miles per hour")
  expect_error(
    validate_safeframe(x, speed = c(
      "character",
      "factor"
    )),
    "- speed: Must inherit from class 'character'/'factor', but has class 'numeric'"
  )

  # Functionalities
  x <- make_safeframe(cars)
  msg <- "`x` has no tags"
  expect_error(validate_safeframe(x), msg)
})
