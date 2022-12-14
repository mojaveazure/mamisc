% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sorted.R
\name{order2}
\alias{order2}
\title{Keyed Ordering Permutation}
\usage{
order2(
  ...,
  key = identity,
  na.last = TRUE,
  decreasing = FALSE,
  method = c("auto", "shell", "radix")
)
}
\arguments{
\item{...}{a sequence of numeric, complex, character or logical
    vectors, all of the same length, or a classed \R object.}

\item{key}{A function that takes a single argument, defaults to
\code{\link[base]{identity}}}

\item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed
    (see \sQuote{Note}.)}

\item{decreasing}{logical.  Should the sort order be increasing or
    decreasing? For the \code{"radix"} method, this can be a vector of
    length equal to the number of arguments in \code{\dots} and the
    elements are recycled as necessary.
    For the other methods, it must be length one.}

\item{method}{the method to be used: partial matches are allowed.  The
    default (\code{"auto"}) implies \code{"radix"} for numeric vectors,
    integer vectors, logical vectors and factors with fewer than
    \eqn{2^{31}}{2^31} elements. Otherwise, it implies \code{"shell"}.
    For details of methods \code{"shell"}, \code{"quick"}, and \code{"radix"},
    see the help for \code{\link[base]{sort}}.}
}
\value{
An integer vector with the ordered indices of the input data
}
\description{
Keyed Ordering Permutation
}
\seealso{
\code{\link{sorted}}
\code{\link[base:sort]{base::sort}}
\code{\link[base:order]{base::order}}
}
