library(dplyr)

test_that("tests for [ operator", {
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  on.exit(lost_labels_action())

  # errors
  lost_labels_action("warning", quiet = TRUE)
  msg <- "The following tagged variables are lost:\n dist - Distance in miles"
  expect_warning(x[, 1], msg)

  lost_labels_action("error", quiet = TRUE)
  msg <- "The following tagged variables are lost:\n dist - Distance in miles"
  expect_error(x[, 1], msg)

  lost_labels_action("warning", quiet = TRUE)
  msg <- "The following tagged variables are lost:\n speed - Miles per hour\n dist - Distance in miles"
  expect_warning(x[, NULL], msg)

  # functionalities
  expect_identical(x, x[])
  expect_identical(x, x[, ])
  expect_null(ncol(x[, 1, drop = TRUE]))
  expect_identical(x[, 1, drop = TRUE], cars[, 1])

  lost_labels_action("none", quiet = TRUE)
  expect_identical(x[, 1], make_safeframe(cars[, 1, drop = FALSE], speed = "Miles per hour"))

  # [ behaves exactly as in the simple data.frame case, including when subset
  # only cols. https://github.com/epiverse-trace/linelist/issues/51
  expect_identical(
    cars[1],
    x[1],
    ignore_attr = TRUE
  )
  expect_identical(
    dplyr::as_tibble(cars)[1],
    x[1],
    ignore_attr = TRUE
  )

  # Warning about drop is surfaced to the user in this situation *iff* not our
  # default
  expect_no_warning(
    x[1]
  )
  expect_warning(
    x[1, drop = FALSE],
    "'drop' argument will be ignored"
  )
  expect_warning(
    dplyr::as_tibble(x)[1, drop = FALSE],
    "`drop` argument ignored"
  )
})

test_that("tests for [<- operator", {
  on.exit(lost_labels_action())

  # errors
  lost_labels_action("warning", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  msg <- "The following tagged variables are lost:\n speed - Miles per hour"
  expect_warning(x[, 1] <- NULL, msg)

  lost_labels_action("error", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  msg <- "The following tagged variables are lost:\n speed - Miles per hour"
  expect_error(x[, 1] <- NULL, msg)

  # functionalities
  x[1:3, 1] <- 1L
  expect_identical(x$speed[1:3], rep(1, 3))

  lost_labels_action("none", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  x[, 1:2] <- NULL
  expect_identical(ncol(x), 0L)

  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  x[, 1] <- "test1"
  # should update the values
  # Should maintain the label
  expect_identical(attr(x$speed, "label"), "Miles per hour")
  attr(x[, 1], "label") <- "Test label assignment 1"
  # should update the label
  expect_identical(attr(x$speed, "label"), "Test label assignment 1")
  # should not activate the lost_action

  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  x[1] <- "test2"
  # should update the values
  # Should maintain the label
  expect_identical(attr(x$speed, "label"), "Miles per hour")

  attr(x[1], "label") <- "Test label assignment 2"
  # Should update the label
  expect_identical(attr(x$speed, "label"), "Test label assignment 2")
})

test_that("[<- allows innocuous label modification", {
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  expect_no_condition(x[1] <- 1L)
  y <- rep(1L, nrow(x))
  attr(y, "label") <- "Miles per hour"
  expect_identical(x$speed, y)
})

test_that("tests for [[<- operator", {
  on.exit(lost_labels_action())

  # errors
  lost_labels_action("warning", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  expect_snapshot_warning(x[[1]] <- NULL)

  lost_labels_action("error", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  expect_snapshot_error(x[[1]] <- NULL)

  # functionalities
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  x[[1]] <- 1L
  y <- rep(1L, nrow(x))
  attr(y, "label") <- "Miles per hour"
  expect_identical(x$speed, y)

  lost_labels_action("none", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  x[[2]] <- NULL
  x[[1]] <- NULL
  expect_identical(ncol(x), 0L)
})

test_that("$<- operator detects label loss", {
  on.exit(lost_labels_action())

  # errors
  lost_labels_action("warning", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  msg <- "The following tagged variables are lost:\n speed - Miles per hour"
  expect_warning(x$speed <- NULL, msg)

  lost_labels_action("error", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  msg <- "The following tagged variables are lost:\n speed - Miles per hour"
  expect_error(x$speed <- NULL, msg)

  lost_labels_action("none", quiet = TRUE)
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  x$speed <- NULL
  x$dist <- NULL
  expect_identical(ncol(x), 0L)
})

test_that("$<- allows innocuous label modification", {
  x <- make_safeframe(cars, speed = "Miles per hour", dist = "Distance in miles")
  expect_no_condition(x$speed <- 1L)
  y <- rep(1L, nrow(x))
  attr(y, "label") <- "Miles per hour"
  expect_identical(x$speed, y)
})

test_that("no warnings when untagged columns are dropped - #55", {
  x <- make_safeframe(cars,
    speed = "Miles per hour"
  )

  expect_silent(x[, "speed"])
})
