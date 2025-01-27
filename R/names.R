#' Rename columns of a safeframe
#'
#' This function can be used to rename the columns a `safeframe` (that is,
#' adjust
#' variable names).
#'
#' @param x a `safeframe` object
#'
#' @param value a `character` vector to set the new names of the columns of `x`
#'
#' @return a `safeframe` with new column names
#'
#' @export
#'
#' @examples
#' ## create safeframe
#' x <- make_safeframe(cars,
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#' head(x)
#'
#' ## change names
#' names(x)[1] <- "mph"
#'
#' ## see results: columns have been updated
#' head(x)
#'
#' # This also works with using `dplyr::rename()` because it uses names<-()
#' # under the hood
#' if (require(dplyr) && require(magrittr)) {
#'   x <- x %>%
#'     rename(speed = "mph")
#'   head(x)
#'   labels(x)
#' }
`names<-.safeframe` <- function(x, value) {
  # Strategy for renaming

  # Since renaming cannot drop columns, we can update labels to match new
  # variable names. We do this by:

  # 1. Storing old names and new names to have define replacement rules
  # 2. Replace all tagged variables using the replacement rules

  out <- drop_safeframe(x)
  names(out) <- value

  # Step 1
  new_names <- names(out)
  if (anyNA(new_names)) {
    stop(
      "Suggested naming would result in `NA` for some column names.\n",
      "Did you provide less names than columns targetted for renaming?",
      call. = FALSE
    )
  }

  # Step 2
  out_labels <- labels(x, show_null = TRUE)
  names(out_labels) <- new_names
  out <- label_variables(out, out_labels)
  class(out) <- class(x)

  out
}
