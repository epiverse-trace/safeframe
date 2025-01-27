#' Extract a data.frame of all tagged variables
#'
#' This function returns a `data.frame`, where tagged variables (as stored in
#' the `safeframe` object) are renamed. Note that the output is no longer a
#' `safeframe`, but a regular `data.frame`. untagged variables are unaffected.
#'
#' @param x a `safeframe` object
#'
#' @export
#'
#' @return A `data.frame` of with variables renamed according to their tags
#'
#' @examples
#'
#' x <- make_safeframe(cars,
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#'
#' ## get a data.frame with variables renamed based on tags
#' tags_df(x)
tags_df <- function(x) {
  checkmate::assertClass(x, "safeframe")

  tags <- unlist(tags(x))
  out <- drop_safeframe(x)

  # Replace the names of out that are in intersection with corresponding tags
  names(out)[match(names(tags), names(out))] <- tags[names(tags)]

  out
}
