#' @include zzz.R
#'
NULL

#' Is an Object a Data Frame
#'
#' @param x An object
#'
#' @return \code{TRUE} if \code{x} is a data frame, otherwise \code{FALSE}
#'
#' @export
#'
is_data_frame <- base::is.data.frame

#' @rdname is_data_frame
#'
#' @export
#'
is_bare_data_frame <- function(x) {
  return(length(x = class(x = x) == 1L) && inherits(x = x, what = 'data.frame'))
}

#' Is a List Named
#'
#' @param x A list
#' @param pass.zero Return \code{TRUE} for zero-length lists
#'
#' @return ...
#'
#' @export
#'
is_named_list <- function(x, pass.zero = FALSE) {
  f <- ifelse(test = isTRUE(x = pass.zero), yes = is_named2, no = is_named)
  return(is_bare_list(x = x) && f(x = x))
}

#' Vectorized Testing of \code{NULL}
#'
#' @param x A list-like object
#'
#' @return ...
#'
#' @export
#'
is_nullv <- function(x) {
  return(vapply(X = x, FUN = is.null, FUN.VALUE = logical(length = 1L)))
}
