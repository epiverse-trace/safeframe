#' Check for lost tags and throw relevant warning or error
#'
#' This internal function checks for tags that are present in the old tags
#' but not in the new tags. If any tags are lost, it throws a warning or
#' error based on the specified action.
#'
#' @param old A named list of old tags.
#' @param new A named list of new tags.
#' @param lost_action A character string specifying the action to take when
#'   tags are lost. Can be "none", "warning", or "error".
#' @keywords internal
#' @return None. Throws a warning or error if tags are lost.
lost_tags <- function(old, new, lost_action) {
  lost_vars <- setdiff(names(old), names(new))

  if (lost_action != "none" && length(lost_vars) > 0) {
    lost_tags <- lapply(lost_vars, function(tag) old[[tag]])

    lost_msg <- vars_tags(lost_vars, lost_tags)
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
