#' @include zzz.R
#'
NULL

#' Interleave vectors together
#'
#' @param ... Vectors to be interleaved
#'
#' @return A vector with the values from each vector in ... interleaved
#'
#' @export
#'
interleave <- function(...) {
  return(as.vector(x = t(x = as.data.frame(x = list(...)))))
}


#' Generate a powerset
#'
#' @param x A list or vector of values to generate all possible combinations of
#'
#' @return A list with all possible combinations of x
#'
#' @importFrom utils combn
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' powerset(x = 1:3)
#' powerset(x = letters[1:3])
#'
powerset <- function(x) {
  x <- unlist(x = x, recursive = TRUE)
  x <- lapply(
    X = 1:length(x = x),
    FUN = function(m) {
      return(as.list(x = as.data.frame(x = combn(x = x, m = m))))
    }
  )
  x <- unlist(x = x, recursive = FALSE, use.names = FALSE)
  x <- lapply(X = x, FUN = as.vector)
  return(x)
}
