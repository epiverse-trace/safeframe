#' A selector function to use in \pkg{tidyverse} functions
#'
#' @param tags A character vector of tags you want to operate on
#'
#' @returns A numeric vector containing the position of the columns with the
#'   requested tags
#'
#' @note Using this in a pipeline results in a 'safeframe' object, but does not
#'   maintain the variable tags at this time. It is primarily useful to make
#'   your pipelines human readable.
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
#' if (require(dplyr) && require(magrittr)) {
#'   x %>%
#'     select(has_tag(c("Miles per hour", "Distance in miles"))) %>%
#'     head()
#' }
has_tag <- function(tags) {
  dat <- tidyselect::peek_data(fn = "has_tag")
  dat_tags <- tags(dat)

  cols_to_extract <- dat_tags[dat_tags %in% tags]

  which(colnames(dat) %in% names(cols_to_extract))
}
