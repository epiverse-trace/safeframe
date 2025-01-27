#' Restore tags of a safeframe
#'
#' Internal. This function is used to restore tags of a `safeframe` object
#' which may have lost its tags after handling for example through `dplyr`
#' verbs. Specific actions can be triggered when some of the tagged variables
#' have disappeared from the object.
#'
#' @param x a `data.frame`
#'
#' @param tags a list of tags as returned by [tags()]; if default values
#' are missing, they will be added to the new list of tags. Matches column
#' names with `x` to restore tags. Throws an error if no matches are found.
#'
#' @param lost_action a `character` indicating the behaviour to adopt when
#'   tagged variables have been lost: "error" (default) will issue an error;
#'   "warning" will issue a warning; "none" will do nothing
#'
#' @noRd
#'
#' @return The function returns a `safeframe` object with updated tags.
#'

restore_tags <- function(x, newTags,
                         lost_action = c("error", "warning", "none")) {
  # assertions
  checkmate::assertClass(x, "data.frame")
  checkmate::assertClass(newTags, "list")
  lost_action <- match.arg(lost_action)

  # Match the remaining variables to the provided tags
  common_vars <- intersect(names(x), names(newTags))
  if (length(common_vars) == 0 && length(names(x)) > 0) {
    stop("No matching tags provided.")
  }

  lost_vars <- setdiff(names(newTags), names(x))

  if (lost_action != "none" && length(lost_vars) > 0) {
    lost_tags <- lapply(lost_vars, function(tag) newTags[[tag]])

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

  for (name in common_vars) {
    attr(x[[name]], "label") <- newTags[[name]]
  }

  # Ensure class consistency
  if (!inherits(x, "safeframe")) {
    class(x) <- c("safeframe", class(x))
  }

  x
}
