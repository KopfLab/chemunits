# install non-standard units
units::install_unit("M", "mol/L", "molarity")

# TODO: update documentation
#' Package options
#' 
#' These options can be set via [options()] and queried via [getOption()].
#' For this, add a `chem.` prefix (the package name and a dot) to the option name.
#' Example: for an option `foo`, use `options(tibble.foo = value)` to set it
#' and `getOption("tibble.foo")` to retrieve the current value.
#' An option value of `NULL` means that the default is used.
#'
#' @examples
#' # Default setting:
#' getOption("tibble.view_max")
#'
#' # Change for the duration of the session:
#' old <- options(tibble.view_max = 100)
#'
#' # view() would show only 100 rows e.g. for a lazy data frame
#'
#' # Change back to the original value:
#' options(old)
#'
#' # Local scope:
#' local({
#'   rlang::local_options(tibble.view_max = 100)
#'   # view() would show only 100 rows e.g. for a lazy data frame
#' })
#' # view() would show the default 1000 rows e.g. for a lazy data frame
#' @export
chemunits_options <- function(...) {
  pkg_options(pkg = "chemunits", pkg_options = .pkg_options, ...)
}

#' @rdname chemunits_options
#' @export 
get_chemunits_option <- function(x) {
  get_pkg_option(option = x, pkg = "chemunits", pkg_options = .pkg_options)
}

# units check function for options
do_units_exist <- function(x) {
  if(!is_character(x)) 
    cli_abort("units must be provided as character vector, 
              not {.obj_type_friendly {x}}")
  sapply(x, set_units, x = 0, mode = "standard")
  return(TRUE)
}

#' @rdname chemunits_options
#' @format NULL
#' @usage NULL
#' @section Options for the chemunits package:
.pkg_options <- list(
  #' - `default_units`: Maximum number of rows shown by [view()]
  #'   if the input is not a data frame, passed on to [head()]. Default: `1000`.
  #'   the order does not matter except if units can be inter converted from each other in which case the first one will be used (with a warning)
  default_units = define_pkg_option(
    default = c("mol", "M", "L", "1"), check_fn = do_units_exist),
  #' - `auto_scale_units`: Maximum number of rows shown by [view()]
  #'   if the input is not a data frame, passed on to [head()]. Default: `1000`.
  auto_scale_units = define_pkg_option(
    default = c("mol", "M", "L"), check_fn = do_units_exist)
)