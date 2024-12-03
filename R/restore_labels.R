#' Restore labels of a safeframe
#'
#' Internal. This function is used to restore labels of a `safeframe` object
#' which may have lost its labels after handling for example through `dplyr`
#' verbs. Specific actions can be triggered when some of the labelled variables
#' have disappeared from the object.
#'
#' @param x a `data.frame`
#'
#' @param labels a list of labels as returned by [labels()]; if default values
#' are missing, they will be added to the new list of labels. Matches column
#' names with `x` to restore labels. Throws an error if no matches are found.
#'
#' @param lost_action a `character` indicating the behaviour to adopt when
#'   labelled variables have been lost: "error" (default) will issue an error;
#'   "warning" will issue a warning; "none" will do nothing
#'
#' @noRd
#'
#' @return The function returns a `safeframe` object with updated labels.
#'

restore_labels <- function(x, newLabels,
                           lost_action = c("error", "warning", "none")) {
  # assertions
  checkmate::assertClass(x, "data.frame")
  checkmate::assertClass(newLabels, "list")
  lost_action <- match.arg(lost_action)

  # Match the remaining variables to the provided labels
  common_vars <- intersect(names(x), names(newLabels))
  if (length(common_vars) == 0 && length(names(x)) > 0) {
    stop("No matching labels provided.")
  }

  lost_vars <- setdiff(names(newLabels), names(x))

  if (lost_action != "none" && length(lost_vars) > 0) {
    lost_labels <- lapply(lost_vars, function(label) newLabels[[label]])

    lost_msg <- vars_labels(lost_vars, lost_labels)
    msg <- paste(
      "The following labelled variables are lost:\n",
      lost_msg
    )
    if (lost_action == "warning") {
      # nolint next: condition_call_linter.
      warning(warningCondition(msg, class = "safeframe_warning"))
    }
    if (lost_action == "error") {
      # nolint next: condition_call_linter.
      stop(errorCondition(msg, class = "safeframe_error"))
    }
  }

  for (name in common_vars) {
    attr(x[[name]], "label") <- newLabels[[name]]
  }

  # Ensure class consistency
  if (!inherits(x, "safeframe")) {
    class(x) <- c("safeframe", class(x))
  }

  x
}
