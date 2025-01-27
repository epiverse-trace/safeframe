#' Check for lost labels and throw relevant warning or error
#'
#' This internal function checks for labels that are present in the old labels
#' but not in the new labels. If any labels are lost, it throws a warning or
#' error based on the specified action.
#'
#' @param old A named list of old labels.
#' @param new A named list of new labels.
#' @param lost_action A character string specifying the action to take when
#'   labels are lost. Can be "none", "warning", or "error".
#' @keywords internal
#' @return None. Throws a warning or error if labels are lost.
lost_labels <- function(old, new, lost_action) {
  lost_vars <- setdiff(names(old), names(new))

  if (lost_action != "none" && length(lost_vars) > 0) {
    lost_labels <- lapply(lost_vars, function(label) old[[label]])

    lost_msg <- vars_labels(lost_vars, lost_labels)
    msg <- paste(
      "The following tagged variables are lost:\n",
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
}
