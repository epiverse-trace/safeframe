#' Add labels to variables
#'
#' Internal. This function will label pre-defined variables in a
#' `data.frame` by adding a label attribute to the column. This can be used for
#' one or multiple variables at the same time.
#'
#' @param x a `data.frame` or a `tibble`, with at least one column
#'
#' @param labels A named list with variable names in `x` as list names and the
#' labels as list values. Values set to `NULL` remove the label.
#'
#' @return The function returns the original object with an additional `"label"`
#'   attribute on each provided variable.
#'
#' @details If used several times, the previous label is removed silently.
#'  Only accepts known variables from the provided `data.frame`.
#'
label_variables <- function(x, labels) {
  # Create an assertion collection to fill with assertions and potential errors
  label_errors <- checkmate::makeAssertCollection()

  # assert_choice() gives clearer error messages than assert_subset() so we
  # use it in a loop with a assertion collection to ensure all issues are
  # returned in the first run.
  vapply(names(labels), FUN = function(namedLabel) {
    checkmate::assert_choice(namedLabel, names(x),
      null.ok = TRUE, add = label_errors
    )
    TRUE
  }, FUN.VALUE = logical(1))

  # Report back on the filled assertion collection
  checkmate::reportAssertions(label_errors)

  # Add the labels to the right location
  # Vectorized approach does not work, so we use a for.. loop instead
  for (name in names(labels)) {
    label_value <- unlist(labels[names(labels) == name])

    # We use the `label` attribute to store the label
    # We use `ifelse` to handle the case where the label is set to `NULL`
    # This is because `as.character(NULL)` returns `character(0)`
    # attr(x[[name]], "label") <- NULL does not have the desired result
    attr(x[[name]], "label") <- ifelse(is.null(label_value),
      "",
      as.character(label_value)
    )
  }

  x
}
