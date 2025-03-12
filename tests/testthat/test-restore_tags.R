test_that("tests for restore_tags", {
  # These are now order dependent for the tests
  x <- make_safeframe(cars, mph = "speed", distance = "dist")
  y <- drop_safeframe(x)
  z <- y
  names(z) <- c("titi", "toto")

  # # Check error messages
  expect_error(
    restore_tags(z, tags(x)),
    "No matching tags provided."
  )

  # Check functionality
  expect_identical(x, restore_tags(x, tags(x)))
  expect_identical(x, restore_tags(y, tags(x)))

  # Classes are correct for different operator use
  expect_equal(class(x), c("safeframe", "data.frame"))
  y <- restore_tags(y, tags(x))
  expect_equal(class(y), c("safeframe", "data.frame"))
  x[[1]] <- "test"
  expect_equal(class(x), c("safeframe", "data.frame"))
  x[1] <- "test2"
  expect_equal(class(x), c("safeframe", "data.frame"))
  x$speed <- "test3"
  expect_equal(class(x), c("safeframe", "data.frame"))
})

test_that("retain class inheritance #56", {
  x <- make_safeframe(
    cars,
    mph = "speed"
  )
  class(x) <- c("linelist", class(x))
  y <- suppressWarnings(x[, 1])

  expect_equal(class(x), class(y))

  # For more complex class inheritance structure, such as a linelist tibble
  x_tbl <- make_safeframe(
    tibble::as_tibble(cars),
    mph = "speed"
  )
  class(x_tbl) <- c("linelist", class(x_tbl))
  y_tbl <- suppressWarnings(x_tbl[, 1])

  expect_equal(class(x_tbl), class(y_tbl))
})
