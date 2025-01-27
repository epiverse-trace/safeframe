#' Printing method for safeframe objects
#'
#' This function prints safeframe objects.
#'
#' @param x a `safeframe` object
#'
#' @param ... further arguments to be passed to 'print'
#'
#' @return Invisibly returns the object.
#'
#' @export
#'
#' @examples
#' ## create safeframe
#' x <- make_safeframe(cars,
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#'
#' ## print object - using only the first few entries
#' head(x)
#'
#' # version with a tibble
#' if (require(tibble) && require(magrittr)) {
#'   cars %>%
#'     tibble() %>%
#'     make_safeframe(
#'       speed = "Miles per hour",
#'       dist = "Distance in miles"
#'     )
#' }
print.safeframe <- function(x, ...) {
  cat("\n// safeframe object\n")
  print(drop_safeframe(x))

  # Extract names and values from labels(x)
  label_values <- unlist(labels(x))
  label_names <- names(label_values)

  # Construct the labels_txt string from the filtered pairs
  labels_txt <- vars_labels(label_names, label_values)

  if (labels_txt == "") {
    cat("\n[no tagged variables]\n")
  } else {
    cat("\ntagged variables:\n", labels_txt, "\n")
  }

  invisible(x)
}
