#' Extract a data.frame of all tagged variables
#'
#' This function returns a `data.frame`, where tagged variables (as stored in
#' the `safeframe` object) are renamed. Note that the output is no longer a
#' `safeframe`, but a regular `data.frame`. Unlabeled variables are unaffected.
#'
#' @param x a `safeframe` object
#'
#' @export
#'
#' @return A `data.frame` of with variables renamed according to their labels.
#'
#' @examples
#'
#' x <- make_safeframe(cars,
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#'
#' ## get a data.frame with variables renamed based on labels
#' labels_df(x)
labels_df <- function(x) {
  checkmate::assertClass(x, "safeframe")

  labels <- unlist(labels(x))
  out <- drop_safeframe(x)

  # Replace the names of out that are in intersection with corresponding labels
  names(out)[match(names(labels), names(out))] <- labels[names(labels)]

  out
}
