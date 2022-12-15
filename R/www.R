#' @include zzz.R
#'
NULL

#' Is a Resource Online and Available
#'
#' Check to see if a given resource is online and accessible over the internet
#'
#' @param uri The URI of a given resource
#' @param strict Ensures the HTTP status code is \code{200}
#' @param timeout Timeout in seconds; defaults to the option
#' \dQuote{\code{timeout}} (currently set to
#' \Sexpr[stage=render]{getOption("timeout")} seconds)
#'
#' @return \code{TRUE} if \code{uri} is accessible, otherwise \code{FALSE}
#'
#' @export
#'
#' @note \Sexpr[stage=build]{mamisc::rd_required_pkgs('httr')}
#'
online <- function(uri, strict = FALSE, timeout = getOption(x = 'timeout')) {
  check_installed(pkg = 'httr')
  if (isTRUE(x = strict)) {
    code <- 200L
    comp <- identical
  } else {
    code <- 404L
    comp <- Negate(f = identical)
  }
  return(comp(
    x = httr::status_code(
      x = httr::GET(url = uri, timeout = httr::timeout(seconds = timeout))
    ),
    y = code
  ))
}
