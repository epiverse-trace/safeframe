#' Add tags to variables
#'
#' Internal. This function will tag pre-defined variables in a
#' `data.frame` by adding a label attribute to the column. This can be used for
#' one or multiple variables at the same time.
#'
#' @param x a `data.frame` or a `tibble`, with at least one column
#'
#' @param tags A named list with variable names in `x` as list names and the
#' tags as list values. Values set to `NULL` remove the tag.
#'
#' @return The function returns the original object with an additional `"label"`
#'   attribute on each provided variable.
#'
#' @details If used several times, the previous tag is removed silently.
#'  Only accepts known variables from the provided `data.frame`.
#'
tag_variables <- function(x, tags) {
  # Create an assertion collection to fill with assertions and potential errors
  tag_errors <- checkmate::makeAssertCollection()

  # assert_choice() gives clearer error messages than assert_subset() so we
  # use it in a loop with a assertion collection to ensure all issues are
  # returned in the first run.
  vapply(names(tags), FUN = function(namedTag) {
    checkmate::assert_choice(namedTag, names(x),
      null.ok = TRUE, add = tag_errors
    )
    TRUE
  }, FUN.VALUE = logical(1))

  # Report back on the filled assertion collection
  checkmate::reportAssertions(tag_errors)

  # Add the tags to the right location
  # Vectorized approach does not work, so we use a for.. loop instead
  for (name in names(tags)) {
    attr(x[[name]], "label") <- tags[[name]]
  }

  x
}
