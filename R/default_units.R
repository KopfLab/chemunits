#' automatically convert to default units if any fit (`defaults` would be an option in units_options)
#' @export
convert_to_default_units <- function(x, defaults = get_chemunits_option("default_units")) {
  # safety check
  stopifnot(
    "`x` has to have units" = !missing(x) && is(x, "units"),
    "`defaults` has to be a scalar" =
      rlang::is_empty(defaults) || rlang::is_character(defaults)
  )
  if (rlang::is_empty(defaults)) {
    return(x)
  }

  # which default units are valid?
  x_units <- base::units(x) |> as.character()
  valid_units <- sapply(defaults, units::ud_are_convertible, x_units, USE.NAMES = FALSE)

  if (sum(valid_units) == 0L) {
    return(x)
  } # no default unit for this quantity
  if (sum(valid_units) > 1L) { # more than one default unit
    sprintf(
      "more than one valid default unit for '%s' - will use '%s' and ignore '%s'",
      x_units, defaults[valid_units][1],
      paste(defaults[valid_units][-1], collapse = "', '")
    ) |>
      warning(immediate. = TRUE, call. = FALSE)
  }

  # convert to default unit
  base::units(x) <- defaults[valid_units][1]
  return(x)
}
