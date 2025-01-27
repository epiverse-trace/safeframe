#' Create a safeframe from a data.frame
#'
#' This function converts a `data.frame` or a `tibble` into a `safeframe`
#' object, where data are tagged and validated. The output will seem to be the
#' same `data.frame`, but `safeframe`-aware packages will then be able to
#' automatically use tagged fields for further data cleaning and analysis.
#'
#' @param x a `data.frame` or a `tibble`
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A named list with variable
#' names in `x` as list names and the labels as list values. Values set to
#' `NULL` remove the label. When specifying labels, please also see
#' `default_values`.
#'
#' @seealso
#'
#' * An overview of the [safeframe] package
#' * [labels()]: for a list of tagged variables in a `safeframe`
#' * [set_labels()]: for modifying labels
#' * [labels_df()]: for selecting variables by labels
#'
#' @export
#'
#' @return The function returns a `safeframe` object.
#'
#' @examples
#'
#' x <- make_safeframe(cars,
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#'
#' ## print result - just first few entries
#' head(x)
#'
#' ## check labels
#' labels(x)
#'
#' ## Labels can also be passed as a list with the splice operator (!!!)
#' my_labels <- list(
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#' new_x <- make_safeframe(cars, !!!my_labels)
#'
#' ## The output is strictly equivalent to the previous one
#' identical(x, new_x)
#'
make_safeframe <- function(x,
                           ...) {
  # assert inputs
  checkmate::assert_data_frame(x, min.cols = 1)
  assert_not_data_table(x)

  labels <- rlang::list2(...)
  x <- label_variables(x, labels)

  # shape output and return object
  class(x) <- c("safeframe", class(x))
  x
}
