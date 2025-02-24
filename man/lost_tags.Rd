% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lost_tags.R
\name{lost_tags}
\alias{lost_tags}
\title{Check for lost tags and throw relevant warning or error}
\usage{
lost_tags(old, new, lost_action)
}
\arguments{
\item{old}{A named list of old tags.}

\item{new}{A named list of new tags.}

\item{lost_action}{A character string specifying the action to take when
tags are lost. Can be "none", "warning", or "error".}
}
\value{
None. Throws a warning or error if tags are lost.
}
\description{
This internal function checks for tags that are present in the old tags
but not in the new tags. If any tags are lost, it throws a warning or
error based on the specified action.
}
\keyword{internal}
