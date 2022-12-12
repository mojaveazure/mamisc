#' @include zzz.R
#'
NULL


#' Find Imported Packages
#'
#' @inheritParams rprojroot::find_package_root_file
#'
#' @return A character vector with all imported packages
#'
#' @keywords internal
#'
#' @export
#'
#' @note \Sexpr[stage=build]{mamisc::rd_required_pkgs('rprojroot')}
#'
ns_pkg_imports <- function(path = '.') {
  check_installed(pkg = 'rprojroot')
  ns <- rprojroot::find_package_root_file('NAMESPACE', path = path)
  nsi <- grep(pattern = '^import', x = readLines(con = ns), value = TRUE)
  nsi <- vapply(
    X = strsplit(x = nsi, split = '(', fixed = TRUE),
    FUN = '[[',
    FUN.VALUE = character(length = 1L),
    2L,
    USE.NAMES = FALSE
  )
  nsi <- vapply(
    X = strsplit(x = nsi, split = ',', fixed = TRUE),
    FUN = '[[',
    FUN.VALUE = character(length = 1L),
    1L,
    USE.NAMES = FALSE
  )
  nsi <- gsub(pattern = '\\)$', replacement = '', x = nsi)
  return(sort(x = unique(x = nsi)))
}
