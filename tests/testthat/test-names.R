test_that("tests for the names<- operator", {
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  old_x <- x
  old_class <- class(x)
  old_names <- names(x)

  # errors
  msg <- paste(
    "Suggested naming would result in `NA` for some column names.",
    "Did you provide less names than columns targetted for renaming?",
    sep = "\n"
  )
  expect_error(names(x) <- "toto", msg)

  # functionalities
  names(x) <- c("titi", "toto")
  expect_named(x, c("titi", "toto"))
  expect_identical(
    tags(x),
    list(titi = "Miles per hour", toto = "Distance in miles")
  )
  expect_s3_class(x, old_class)
  names(x) <- old_names
  expect_identical(x, old_x)
})
