#' Keyed Ordering Permutation
#'
#' @inheritParams base::order
#' @inheritParams sorted
#'
#' @return An integer vector with the ordered indices of the input data
#'
#' @export
#'
#' @seealso \code{\link{sorted}}
#' \code{\link[base:sort]{base::sort}}
#' \code{\link[base:order]{base::order}}
#'
order2 <- function(
  ...,
  key = identity,
  na.last = TRUE,
  decreasing = FALSE,
  method = c('auto', 'shell', 'radix')
) {
  z <- list(...)
  z <- sapply(X = z, FUN = key, USE.NAMES = FALSE)
  method <- match.arg(arg = method)
  return(order(z, na.last = na.last, decreasing = decreasing, method = method))
}

#' Sort With A Key
#'
#' @inheritParams base::sort
#' @param key A function that takes a single argument, defaults to
#' \code{\link[base]{identity}}
#'
#' @export
#'
#' @seealso \code{\link{order2}}
#' \code{\link[base:sort]{base::sort}}
#' \code{\link[base:order]{base::order}}
#'
sorted <- function(x, key = identity, decreasing = FALSE, ...) {
  if (length(x = formals(fun = key)) != 1) {
    stop("'key' must be a function that takes a single argument")
  }
  UseMethod(generic = 'sorted', object = x)
}

#' @rdname sorted
#' @method sorted default
#' @export
#'
sorted.default <- function(
  x,
  key = identity,
  decreasing = FALSE,
  na.last = NA,
  ...
) {
  return(x[order2(x, key = key, na.last = na.last, decreasing = decreasing, ...)])
}
