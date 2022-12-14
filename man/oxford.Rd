% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{oxford}
\alias{oxford}
\title{Create a List with a Serial Comma}
\usage{
oxford(
  ...,
  cnj = c("or", "and"),
  quote = c("single", "double", "none"),
  fancy = getOption(x = "useFancyQuotes", default = TRUE)
)
}
\arguments{
\item{...}{A character vector to join}

\item{cnj}{Conjunction to use for final entry}

\item{quote}{Quote the entries of \code{...}; choose from:
\itemize{
 \item \dQuote{\code{single}}: single quotes
 \item \dQuote{\code{double}}: double quotes
 \item \dQuote{\code{none}}: no extra quoting
}}

\item{fancy}{Use fancy quotes; defaults to the value of the
\dQuote{\code{useFancyQuotes}} option (currently set to
\Sexpr[stage=render]{getOption(x = "useFancyQuotes", default = TRUE)})}
}
\value{
\code{...} arranged into an English list with a serial comma
when needed
}
\description{
Create a List with a Serial Comma
}
\examples{
oxford('cell')
oxford('cell', 'ident')
oxford('cell', 'ident', 'gene')

}
\seealso{
\code{\link[base]{sQuote}()} \code{\link[base]{dQuote}()}
}
