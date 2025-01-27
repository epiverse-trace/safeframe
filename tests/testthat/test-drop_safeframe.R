test_that("tests for drop_safeframe", {
  x <- make_safeframe(cars, speed = "Miles per hour")
  expect_identical(cars, drop_safeframe(x, remove_tags = TRUE))

  y <- drop_safeframe(x, remove_tags = FALSE)
  expect_identical(tags(x, TRUE)$speed, attr(y$speed, "label"))
  expect_identical(tags(x, TRUE)$dist, attr(y$dist, "label"))
})
