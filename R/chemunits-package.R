#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The chemunits package augments the units package for use in chemistry calculations.
#'
#' Resources:
#'   * Website for the chemunits package: <https://chemunits.kopflab.org>
#'   * Create a chemunit: [set_cu()]
#'   * Package options: [chemunits_options]
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @import units
#' @import vctrs
#' @importFrom lifecycle deprecated
#' @importFrom methods is
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# re-exports

#' @export
tibble::tibble

# note: because `chemunits` has `units` as Depends, all units functions are automatically available to the user and do not need to be re-exported here
