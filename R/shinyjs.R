#' @include zzz.R
#'
NULL

#' Hide Shiny Tabs with \pkg{ShinyJS}
#'
#' @inheritParams js_tab_key
#' @param fxn Name of JavaScript call function
#'
#' @return A string with a JavaScript function to hide a set of tabs
#'
#' @export
#'
#' @note This function is designed to run custom JavaScript code using
#' \code{\link[shinyjs:extendShinyJS]{shinyjs::extendShinyJS}()}; use of
#' custom JavaScript code requires the \pkg{V8} package.
#' \pkg{V8} requires a local install of either the
#' \href{https://chromium.googlesource.com/v8/v8}{V8} JavaScript Engine or
#' \href{https://nodejs.org/en/}{Node.js}
#'
#' @seealso \code{\link[shinyjs:extendShinyJS]{shinyjs::extendShinyJS}()}
#' \code{\link[shiny:tabPanel]{shiny::tabPanel}()}
#'
#' @family shiny
#' @family shinyjs
#'
js_tab_hide <- function(id, values, fxn = 'hide') {
  keys <- paste0(
    '$(',
    sQuote(x = js_tab_key(id = id, values = values), q = FALSE),
    ').hide()',
    collapse = '; '
  )
  return(paste0('shinyjs.', fxn, ' = function() {', keys, ';}'))
}

#' Get JavaScript IDs for Shiny Tabs
#'
#' @param id ID of a \code{\link[shiny]{tabsetPanel}}
#' @param values One or more values of a \code{\link[shiny]{tabPanel}}
#' (see the \code{value} parameter)
#'
#' @return A string with the JavaScript ID for a given set of tabs
#'
#' @export
#'
#' @seealso \code{\link[shiny:tabsetPanel]{shiny::tabsetPanel}()}
#' \code{\link[shiny:tabPanel]{shiny::tabPanel}()}
#'
#' @family shiny
#' @family shinyjs
#'
js_tab_key <- function(id, values) {
  return(paste0(
    '#',
    id,
    ' li a[data-value=',
    dQuote(x = values, q = FALSE),
    ']'
  ))
}
