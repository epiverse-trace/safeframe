test_that("tests for restore_labels", {
  # These are now order dependent for the tests
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  y <- drop_safeframe(x)
  z <- y
  names(z) <- c("titi", "toto")

  # Check error messages
  expect_error(
    restore_labels(z, labels(x)),
    "No matching labels provided."
  )

  # Check functionality
  expect_identical(x, restore_labels(x, labels(x)))
  expect_identical(x, restore_labels(y, labels(x)))

  # Classes are correct for different operator use
  expect_equal(class(x), c("safeframe", "data.frame"))
  y <- restore_labels(y, labels(x))
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
    speed = "Miles per hour"
  )
  class(x) <- c("linelist", class(x))
  class(x)
  #> [1] "linelist"   "safeframe"  "data.frame"
  
  y <- suppressWarnings(x[, 1])
  #> Warning: The following labelled variables are lost:
  #>  dist - NULL
  class(y)
  
  expect_equal(class(x), class(y))
})
