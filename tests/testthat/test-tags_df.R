test_that("tests for tags_df without untagged variables", {
  # These are now order dependent for the tests
  x <- make_safeframe(cars,
    speed = "Miles per hour",
    dist = "Distance in miles"
  )
  y <- cars[c("speed", "dist")]
  names(y) <- c("Miles per hour", "Distance in miles")

  # errors
  msg <- "Must inherit from class 'safeframe', but has class 'data.frame'."
  expect_error(tags_df(cars), msg)

  # functionality
  expect_identical(tags_df(x), y)
})


test_that("tags_df with untagged variables works as expected", {
  x <- make_safeframe(cars,
    dist = "Distance in miles"
  )
  y <- cars[c("speed", "dist")]
  names(y) <- c("speed", "Distance in miles")

  expect_identical(tags_df(x), y)
})
