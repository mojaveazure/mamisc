#' Rdocumentation Itemized List
#'
#' Generate an Rdocumentation itemized list
#'
#' @param x A vector of values for the list
#'
#' @return An Rdocumentation itemized list suitable for injecting into
#' documentation files with \code{\\Sexpr}
#'
#' @templateVar fname rd_ilist
#'
#' @keywords documentation
#'
#' @export
#'
#' @family roxygen
#'
#' @template example-rd-inject
#'
rd_ilist <- function(x) {
  return(paste(
    "\\itemize{",
    paste0(' \\item \\dQuote{\\code{', x, "}}", collapse = '\n'),
    "}",
    sep = '\n'
  ))
}

#' Rdocumentation Note of Required Packages
#'
#' @param x A vector of package names
#' @param multiple ...
#'
#' @return ...
#'
#' @templateVar fname rd_required_pkgs
#'
#' @export
#'
#' @family roxygen
#'
#' @template example-rd-inject
#'
rd_required_pkgs <- function(x, multiple = FALSE) {
  url <- paste0(
    '\\href{https://cran.r-project.org/package=',
    x,
    '}{\\pkg{',
    x,
    '}}'
  )
  if (length(x = url) > 2L) {
    url <- c(
      paste0(paste(url[1:length(x = url) - 1L], collapse = ', '), ','),
      url[length(x = url)]
    )
  }
  if (length(x = url) == 2L) {
    url <- paste(url, collapse = ' and ')
  }
  return(paste(
    ifelse(
      test = isTRUE(x = multiple),
      yes = 'These functions',
      no = 'This function'
    ),
    'requires the',
    url,
    ifelse(test = length(x = x) == 1L, yes = 'package', no = 'packages'),
    'to be installed'
  ))
}
