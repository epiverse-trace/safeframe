#' Tag a variable using its name
#'
#' Internal. This function will tag pre-defined variables in a
#' `data.frame` by adding a label attribute to the column. This can be used for
#' one or multiple variables at the same time.
#'
#' @param x a `data.frame` or a `tibble`, with at least one column
#'
#' @param tags A named list with tag names as list names and names or positions
#' of columns to tag as list values. Values can be `NULL` to remove a tag.
#'
#' @return The function returns the original object with an additional `"label"`
#'   attribute on each provided variable.
#'
#' @details If used several times, the previous tag is removed silently.
#'  Only accepts known variables from the provided `data.frame`.
#'
#' @noRd

tag_variables <- function(x, tags) {
  # Create an assertion collection to fill with assertions and potential errors
  tag_errors <- checkmate::makeAssertCollection()

  by_position <- vapply(tags, is.numeric, logical(1))
  if (any(unlist(tags[by_position]) > ncol(x))) {
    stop(
      "Tags specified by position must be lower than the number of columns.",
      call. = FALSE
    )
  }
  tags[by_position] <- names(x)[unlist(tags[by_position])]

  # assert_choice() gives clearer error messages than assert_subset() so we
  # use it in a loop with a assertion collection to ensure all issues are
  # returned in the first run.
  lapply(tags, function(tag) {
    checkmate::assert_choice(tag, names(x), null.ok = TRUE, add = tag_errors)
  })
  checkmate::reportAssertions(tag_errors)

  # Add the tags to the right location
  # Vectorized approach does not work, so we use a for.. loop instead
  for (tag in names(tags)) {
    var <- tags[[tag]]
    if (is.null(var)) {
      # Find the relevant variable for the tag without a variable
      removeVar <- tags(x)[[tag]]
      if (length(removeVar) > 0) {
        # Remove the tag on the var
        x <- remove_tag(x, removeVar[[1]])
      }
    } else {
      attr(x[[var]], "label") <- tag
    }
  }

  x
}
