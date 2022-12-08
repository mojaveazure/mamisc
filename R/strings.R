#' @include zzz.R
#'
NULL

#' Create Abbreviations
#'
#' @param x A character vector
#' @param digits Include digits in the abbreviation
#'
#' @return Abbreviated versions of \code{x}
#'
#' @export
#'
#' @examples
#' abbrv(c('HelloWorld', 'LetsGo3', 'tomato'))
#' abbrv(c('HelloWorld', 'LetsGo3', 'tomato'), digits = FALSE)
#' abbrv('Wow3', digits = FALSE)
#'
abbrv <- function(x, digits = TRUE) {
  pattern <- ifelse(test = isTRUE(x = digits), yes = '[A-Z0-9]+', no = '[A-Z]+')
  y <- vapply(
    X = regmatches(x = x, m = gregexec(pattern = pattern, text = x)),
    FUN = paste,
    FUN.VALUE = character(length = 1L),
    collapse = ''
  )
  na <- nchar(x = y) <= 1L
  y[na] <- x[na]
  return(tolower(x = y))
}

#' Create a List with a Serial Comma
#'
#' @param ... A character vector to join
#' @param cnj Conjunction to use for final entry
#' @param quote Quote the entries of \code{...}; choose from:
#' \itemize{
#'  \item \dQuote{\code{single}}: single quotes
#'  \item \dQuote{\code{double}}: double quotes
#'  \item \dQuote{\code{none}}: no extra quoting
#' }
#' @param fancy Use fancy quotes; defaults to the value of the
#' \dQuote{\code{useFancyQuotes}} option (currently set to
#' \Sexpr[stage=render]{getOption(x = "useFancyQuotes", default = TRUE)})
#'
#' @return \code{...} arranged into an English list with a serial comma
#' when needed
#'
#' @export
#'
#' @seealso \code{\link[base]{sQuote}()} \code{\link[base]{dQuote}()}
#'
#' @examples
#' oxford('cell')
#' oxford('cell', 'ident')
#' oxford('cell', 'ident', 'gene')
#'
oxford <- function(
  ...,
  cnj = c('or', 'and'),
  quote = c('single', 'double', 'none'),
  fancy = getOption(x = 'useFancyQuotes', default = TRUE)
) {
  x <- as.character(x = c(...))
  cnj <- arg_match(arg = cnj)
  quote <- if (isTRUE(x = quote)) {
    'double'
  } else if (is_na(x = quote)) {
    'single'
  } else if (isFALSE(x = quote)) {
    'none'
  } else {
    quote
  }
  quote <- arg_match(arg = quote)
  x <- switch(
    EXPR = quote,
    single = sQuote(x = x, q = isTRUE(x = fancy)),
    double = dQuote(x = x, q = isTRUE(x = fancy)),
    x
  )
  if (length(x = x) > 2L) {
    x <- c(
      paste0(paste(x[1:(length(x = x) - 1L)], collapse = ', '), ','),
      x[length(x = x)]
    )
  }
  if (length(x = x) == 2L) {
    x <- paste(x, collapse = paste0(' ', cnj, ' '))
  }
  return(x)
  # if (length(x = x) <= 1L) {
  #   return(x)
  # } else if (length(x = x) == 2L) {
  #   return(paste(x, collapse = paste0(' ', cnj, ' ')))
  # }
  # return(paste(
  #   paste0(paste(x[1:(length(x = x) - 1L)], collapse = ', '), ','),
  #   cnj,
  #   x[length(x = x)]
  # ))
}
