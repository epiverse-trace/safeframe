test_that("label_variables() fails for non-existing variables", {
  msg <- "* Variable 'namedLabel': Must be element of set {'speed','dist'}, but"
  expect_error(
    label_variables(cars, list(distance = "toto")),
    msg,
    fixed = TRUE
  )
})

test_that("label_variables() succeeds in various scenarios", {
  x <- label_variables(cars, list(dist = "Distance in miles"))
  expect_identical(attr(x$dist, "label"), "Distance in miles")
  # Expect NULL because this attribute has not been set at all
  expect_identical(attr(x$speed, "label"), NULL)

  x <- label_variables(x, list(speed = "vitesse"))
  expect_identical(attr(x$speed, "label"), "vitesse")
  expect_identical(attr(x$dist, "label"), "Distance in miles")

  # reset to NULL
  x <- label_variables(x, list(speed = NULL, dist = NULL))
  expect_null(attr(x$speed, "label"))
  expect_null(attr(x$dist, "label"))
})
