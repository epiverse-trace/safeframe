#' Change labels of a safeframe object
#'
#' This function changes the `labels` of a `safeframe` object, using the same
#' syntax as the constructor [make_safeframe()].
#'
#' @inheritParams make_safeframe
#'
#' @seealso [make_safeframe()] to create a `safeframe` object
#'
#' @export
#'
#' @return The function returns a `safeframe` object.
#'
#' @examples
#'
#' ## create a safeframe
#' x <- make_safeframe(cars, speed = "Miles per hour")
#' labels(x)
#'
#' ## add new labels and fix an existing one
#' x <- set_labels(x, dist = "Distance")
#' labels(x)
#'
#' ## remove labels by setting them to NULL
#' old_labels <- labels(x)
#' x <- set_labels(x, speed = NULL, dist = NULL)
#' labels(x)
#'
#' ## setting labels providing a list (used to restore old labels here)
#' x <- set_labels(x, !!!old_labels)
#' labels(x)
set_labels <- function(x, ...) {
  # assert inputs
  checkmate::assertClass(x, "safeframe")

  # For some reason, we cannot remove labels from safeframe objects by setting
  # the attr to NULL.
  # We circumvent the issue by:
  # 1. saving the existing labels
  # 2. dropping all labels & removing the safeframe class
  # 3. readding the labels and the safeframe class
  
  new_labels <- rlang::list2(...)
  existing_labels <- labels(x)
  
  x <- drop_safeframe(x, remove_labels = TRUE)

  x <- label_variables(x, utils::modifyList(existing_labels, new_labels))

  # shape output and return object
  class(x) <- c("safeframe", class(x))

  x
}
