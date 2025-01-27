test_that("tests for make_safeframe", {
  # test errors
  msg <- "Must be of type 'data.frame', not 'NULL'."
  expect_error(make_safeframe(NULL), msg)

  msg <- "Must have at least 1 cols, but has 0 cols."
  expect_error(make_safeframe(data.frame()), msg)

  msg <- "* Variable 'namedTag': Must be element of set {'speed','dist'}, but"
  expect_error(make_safeframe(cars, outcome = "bar"), msg, fixed = TRUE)

  expect_error(
    make_safeframe(cars, outcome = "bar", age = "bla"),
    "2 assertions failed"
  )

  # test functionalities
  expect_identical(
    list(speed = NULL, dist = NULL),
    tags(make_safeframe(cars), TRUE)
  )

  x <- make_safeframe(cars, dist = "Date onset", speed = "Date outcome")
  expect_identical(tags(x)$dist, "Date onset")
  expect_identical(tags(x)$speed, "Date outcome")
  expect_null(tags(x)$"Date onset")
  expect_null(tags(x)$"Date outcome")

  x <- make_safeframe(cars, speed = "foo", dist = "bar")
  expect_identical(
    tags(x, TRUE),
    c(list(), speed = "foo", dist = "bar")
  )
})

test_that("make_safeframe() works with dynamic dots", {
  expect_identical(
    make_safeframe(cars, dist = "date_onset", speed = "date_outcome"),
    make_safeframe(cars, !!!list(dist = "date_onset", speed = "date_outcome"))
  )
})

test_that("make_safeframe() errors on data.table input", {
  dt_cars <- structure(
    cars,
    class = c("data.table", "data.frame")
  )

  expect_error(
    make_safeframe(dt_cars),
    "NOT be a data.table"
  )
})

test_that("make_safeframe() works with single & multi-word tags", {
  expect_no_condition(make_safeframe(cars, speed = "mph"))
  expect_no_condition(make_safeframe(cars, speed = "Miles per hour"))
})
