#' Base Tools for Labelling and Validating Data
#'
#' The \pkg{safeframe} package provides tools to help label and validate data.
#' The 'safeframe' class adds column level attributes to a 'data.frame'.
#' Once tagged, variables can be seamlessly used in downstream analyses,
#' making data pipelines more robust and reliable.
#'
#' @aliases safeframe
#'
#' @section Main functions:
#'
#'   * [make_safeframe()]: to create `safeframe` objects from a `data.frame` or
#'   a `tibble`
#'
#'   * [set_labels()]: to change or add tagged variables in a `safeframe`
#'
#'   * [labels()]: to get the list of labels of a `safeframe`
#'
#'   * [labels_df()]: to get a `data.frame` of all tagged variables
#'
#'   * [lost_labels_action()]: to change the behaviour of actions where tagged
#'   variables are lost (e.g removing columns storing tagged variables) to
#'   issue warnings, errors, or do nothing
#'
#'   * [get_lost_labels_action()]: to check the current behaviour of actions
#'   where tagged variables are lost
#'
#' @section Dedicated methods:
#'
#'   Specific methods commonly used to handle `data.frame` are provided for
#'   `safeframe` objects, typically to help flag or prevent actions which could
#'   alter or lose tagged variables (and may thus break downstream data
#'   pipelines).
#'
#'   * `names() <-` (and related functions, such as [dplyr::rename()]) will
#'   rename labels as needed
#'
#'   * `x[...] <-` and `x[[...]] <-` (see [sub_safeframe]): will adopt the
#'    desired behaviour when tagged variables are lost
#'
#'   * `print()`: prints info about the `safeframe` in addition to the
#'   `data.frame` or `tibble`
#'
#' @note The package does not aim to have complete integration with \pkg{dplyr}
#' functions. For example, [dplyr::mutate()] and [dplyr::bind_rows()] will
#' not preserve labels. We only provide compatibility for [dplyr::rename()].
#'
#' @examples
#'
#' # using base R style
#' x <- make_safeframe(cars[1:50, ],
#'   speed = "Miles per hour",
#'   dist = "Distance in miles"
#' )
#' x
#'
#' ## check tagged variables
#' labels(x)
#'
#' ## robust renaming
#' names(x)[1] <- "identifier"
#' x
#'
#' ## example of dropping labels by mistake - default: warning
#' x[, 2]
#'
#' ## to silence warnings when labels are dropped
#' lost_labels_action("none")
#' x[, 2]
#'
#' ## to trigger errors when labels are dropped
#' # lost_labels_action("error")
#' # x[, 2:5]
#'
#' ## reset default behaviour
#' lost_labels_action()
#'
#' # using tidyverse style
#'
#' ## example of creating a safeframe, adding a new variable, and adding a label
#' ## for it
#'
#' if (require(dplyr) && require(magrittr)) {
#'   x <- cars %>%
#'     tibble() %>%
#'     make_safeframe(
#'       speed = "Miles per hour",
#'       dist = "Distance in miles"
#'     ) %>%
#'     mutate(result = if_else(speed > 50, "fast", "slow")) %>%
#'     set_labels(result = "Ticket yes/no")
#'
#'   head(x)
#'
#'   ## extract tagged variables
#'   x %>%
#'     select(has_label(c("Ticket yes/no")))
#'
#'   ## Retrieve all labels
#'   x %>%
#'     labels()
#'
#'   ## Select based on variable name
#'   x %>%
#'     select(starts_with("speed"))
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
