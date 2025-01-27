#' Get the list of tags in a safeframe
#'
#' This function returns the list of tags identifying specific variable types
#' in a `safeframe` object.
#'
#' @param x a `safeframe` object
#'
#' @param show_null a `logical` indicating if the complete list of tags,
#'   including `NULL` ones, should be returned; if `FALSE`, only tags with a
#'   non-NULL value are returned; defaults to `FALSE`
#'
#' @export
#'
#' @return The function returns a named `list` where names indicate which column
#' they correspond to, and values indicate the relevant tags.
#'
#' @details tags are stored as the `label` attribute of the column variable.
#'
#' @examples
#'
#' ## make a safeframe
#' x <- make_safeframe(cars, speed = "Miles per hour")
#'
#' ## check non-null tags
#' tags(x)
#'
#' ## get a list of all tags, including NULL ones
#' tags(x, TRUE)
tags <- function(x, show_null = FALSE) {
  checkmate::assertClass(x, "safeframe")
  out <- lapply(names(x), FUN = function(var) {
    attr(x[[var]], "label")
  })
  names(out) <- names(x)

  # Filter out NULL values if show_null is FALSE
  if (!show_null) {
    out <- Filter(Negate(is.null), out)
  }

  out
}
