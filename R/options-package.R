# TODO: update documentation
#' Package options
#'
#' These options are best set via [chemunits_options()] and queried via [get_chemunits_option()]. However, the base functions [options()] and [getOption()] work as well but require a `chemunits.` prefix (the package name and a dot) for the option name. Setting an option to a value of `NULL` means that the default is used.
#'
#' @examples
#' # Default setting(s):
#' old <- chemunits_options()
#' old # original options
#' get_chemunits_option("preferred_units")
#'
#' # with this, there is no default unit defined for energy or volume
#' # so the units are not simplified
#' set_cu(100, "W") * set_cu(1, "s")
#' set_cu(1, "m") * set_cu(1, "mm^2")
#' 
#' # however the default does simplify dimensionless units
#' set_cu(1, "W*s") / set_cu(1, "J")
#'
#' # Change for the duration of the session:
#' chemunits_options(preferred_units = c("J", "L"))
#'
#' # now W*s is recognized as energy and cm mm^2 as a volume and both are
#' # automatically converted to the specified default units
#' set_cu(100, "W") * set_cu(1, "s")
#' set_cu(1, "m") * set_cu(1, "mm^2")
#' 
#' # however since "1" was no longer included in the `preferred_units`, 
#' # dimensionless units were no longer simplified (thus it is advisable
#' # to always include "1" in the `preferred_units` option)
#' set_cu(1, "W*s") / set_cu(1, "J")
#'
#' # Restore original values
#' chemunits_options(old)
#'
#' @param ... set package options, syntax identical to [options()]
#' @describeIn chemunits_options set option values
#' @export
chemunits_options <- function(...) {
  pkg_options(pkg = "chemunits", pkg_options = get_pkg_options(), ...)
}


#' @describeIn chemunits_options retrieve the current value of an option
#' @param x name of the option to retrieve
#' @export
get_chemunits_option <- function(x) {
  get_pkg_option(option = x, pkg = "chemunits", pkg_options = get_pkg_options())
}

#' @rdname chemunits_options
#' @format NULL
#' @usage NULL
#' @section Options for the chemunits package:
get_pkg_options <- function() {
  list(
    #' - `preferred_units` (character vector of units): the default units that new units (i.e. resulting from calculations) should be converted to. Default: `"1"` (i.e. units that cancel themselves and should be dimensionless are converted to unitless). The order does not matter except if units can be interconverted from each other (e.g. `"L"` and `"m^3"`) in which case the first one will be used (with a warning).
    preferred_units = define_pkg_option(
      default = c("mol", "g", "g/mol", "M/bar", "M", "L", "1"), check_fn = do_units_exist
    ),
    #' - `auto_scale_units` (character vector of units): the units whose best SI prefix (e.g. nano/n, micro/u, milli/m, kilo/k, etc) should be determined automatically based on the median value of a vector in this unit. Default: `character(0)` (i.e. no units are automatically scaled).
    auto_scale_units = define_pkg_option(
      default = c("mol", "g", "g/mol", "M/bar", "M", "L"), check_fn = do_units_exist
    )
  )
}

# units check function for options
do_units_exist <- function(x) {
  if (!is_character(x)) {
    cli_abort("units must be provided as character vector,
              not {.obj_type_friendly {x}}")
  }
  sapply(x, set_units, x = 0, mode = "standard")
  return(TRUE)
}
