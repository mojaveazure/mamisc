% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sorted.R
\name{sorted}
\alias{sorted}
\alias{sorted.default}
\title{Sort With A Key}
\usage{
sorted(x, key = identity, decreasing = FALSE, ...)

\method{sorted}{default}(x, key = identity, decreasing = FALSE, na.last = NA, ...)
}
\arguments{
\item{x}{for \code{sort} an \R object with a class or a numeric,
    complex, character or logical vector.  For \code{sort.int}, a
    numeric, complex, character or logical vector, or a factor.}

\item{key}{A function that takes a single argument, defaults to
\code{\link[base]{identity}}}

\item{decreasing}{logical.  Should the sort be increasing or decreasing?
    Not available for partial sorting.}

\item{...}{arguments to be passed to or from methods or (for the
    default methods and objects without a class) to \code{sort.int}.}

\item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
}
\description{
Sort With A Key
}
\seealso{
\code{\link{order2}}
\code{\link[base:sort]{base::sort}}
\code{\link[base:order]{base::order}}
}
