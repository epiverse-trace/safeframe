#' Type check variables
#'
#' This function checks the type of variables in a `safeframe` against
#' accepted classes. Only checks the type of provided variables and ignores
#' those not provided.
#'
#' @export
#'
#' @param x a `safeframe` object
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A named list with variable
#' names in `x` as list names and the related types as list values.
#'
#' @return A named `list`.
#'
#' @seealso
#' * [validate_tags()] to perform a series of checks on variables
#' * [validate_safeframe()] to combine `validate_tags` and `validate_types`
#'
#' @examples
#' x <- make_safeframe(cars,
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#' x
#'
#' ## the below would issue an error
#' ## note: tryCatch is only used to avoid a genuine error in the example
#' tryCatch(validate_types(x), error = paste)
#'
#' ## to allow other types, e.g. gender to be integer, character or factor
#' validate_types(x, speed = "numeric", dist = c(
#'   "integer",
#'   "character", "numeric"
#' ))
#'
validate_types <- function(x, ...) {
  checkmate::assert_class(x, "safeframe")
  types <- rlang::list2(...)
  checkmate::assert_list(types, min.len = 1, types = "character")

  vars_to_check <- intersect(names(x), names(types))

  type_checks <- lapply(
    vars_to_check,
    function(var) {
      allowed_types <- types[[var]]
      checkmate::check_multi_class(
        x[[var]],
        allowed_types,
        null.ok = TRUE
      )
    }
  )
  has_correct_types <- vapply(type_checks, isTRUE, logical(1))

  if (!all(has_correct_types)) {
    stop(
      "Some tags have the wrong class:\n",
      sprintf(
        "  - %s: %s\n",
        vars_to_check[!has_correct_types],
        type_checks[!has_correct_types]
      ),
      call. = FALSE
    )
  }

  x
}
