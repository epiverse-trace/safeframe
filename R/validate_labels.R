#' Checks the labels of a safeframe object
#'
#' This function evaluates the validity of the labels of a `safeframe` object by
#' checking that: i) labels are present ii) labels is a `list` of `character` or
#' `NULL` values.
#'
#' @export
#'
#' @param x a `safeframe` object
#'
#' @return If checks pass, a `safeframe` object; otherwise issues an error.
#'
#' @seealso [validate_types()] to check if tagged variables have
#'   the right classes
#'
#' @examples
#' ## create a valid safeframe
#' x <- cars |>
#'   make_safeframe(
#'     speed = "Miles per hour",
#'     dist = "Distance in miles"
#'   )
#' x
#'
#' ## the below issues an error as safeframe doesn't know any defaults
#' ## note: tryCatch is only used to avoid a genuine error in the example
#' tryCatch(validate_safeframe(x), error = paste)
#'
#' ## validation requires you to specify the types directly
#' validate_safeframe(x,
#'   speed = c("integer", "numeric"),
#'   dist = "numeric"
#' )
validate_labels <- function(x) {
  checkmate::assert_class(x, "safeframe")
  x_labels <- labels(x, show_null = TRUE)

  if (is.null(unlist(x_labels))) stop("`x` has no labels")

  # check that x_labels is a list, and each label is a `character`
  checkmate::assert_list(x_labels, types = c("character", "null"))

  x
}
