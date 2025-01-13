#' automatically scale numerators to the best udunits prefix if x has units
#' that are listed in the scalable_units
#' @export
auto_scale_units <- function(
    x, scalable_units = get_chemunits_option("auto_scale_units")) {
  check_required(x)
  check_required(scalable_units)
  scaled_units <- find_scalable_units(x, scalable_units)
  if (!is.null(scaled_units)) units(x) <- scaled_units
  return(x)
}

# check if x has autoscalable units in the numerator
find_scalable_units <- function(
    x,
    scalable_units = get_chemunits_option("auto_scale_units"),
    call = caller_env()) {
  if (!is(x, "units")) {
    cli_abort("{.var x} must have units but is {.obj_type_friendly {x}}",
      call = call
    )
  }

  if (is_empty(scalable_units)) {
    return(NULL)
  } # no scalable units at all

  if (!is_character(scalable_units)) {
    cli_abort("{.var scalable_units} must be a character vector instead
              of {.obj_type_friendly {scalable_units}}",
      call = call
    )
  }

  x_num <- units(x)$numerator
  if (length(x_num) == 0L) {
    return(NULL)
  } # x has no numerator

  auto_scale <- lapply(scalable_units, function(u) grepl(paste0(u, "$"), x_num))
  auto_scale_matches <- sapply(auto_scale, sum)
  if (sum(auto_scale_matches) == 0L) {
    return(NULL)
  } # none are scalable

  auto_scale_unit <- scalable_units[auto_scale_matches == 1]
  if (length(auto_scale_unit) > 1L) { # more than one unit is scalable
    full_unit <- paste(x_num, collapse = "*") |>
      paste(units(x)$denominator, collapse = "/")
    cli_warn(
      c("there is ambiguity which unit to autoscale,
        picking the first ({.emph {auto_scale_unit[1]}})",
        "i" = "units: {.emph {full_unit}},
      numerator units {.emph {auto_scale_unit}} can be autoscaled"
      ),
      call = call
    )
    auto_scale_unit <- auto_scale_unit[1]
  }

  # SI prefixes (could get these from units::valid_udunits_prefixes(
  # but the xml dependency just for that seems silly)
  prefixes <- c(
    "y" = -24, "z" = -21, "a" = -18, "f" = -15,
    "p" = -12, "n" = -9, "\U00B5" = -6, "u" = -6, "m" = -3,
    "NONE" = 0, "k" = 3, "M" = 6, "G" = 9, "T" = 12,
    "P" = 15, "E" = 18, "Z" = 21, "Y" = 24
  )

  # get current prefix
  x_num_idx <- which(auto_scale[auto_scale_matches == 1][[1]])
  current_prefix <- sub(paste0(auto_scale_unit, "$"), "", x_num[x_num_idx])
  current_prefix <- if (current_prefix == "") "NONE" else current_prefix
  if (!current_prefix %in% names(prefixes)) { # should be impossible but just for safety
    sprintf(
      "not a known metric prefix '%s' in numerator unit '%s' - will NOT autoscale",
      current_prefix, x_num[x_num_idx]
    ) |>
      warning(immediate. = TRUE, call. = FALSE)
    return(x)
  }

  # get best prefix for auto-scaling
  best_scale_factor <- prefixes[[current_prefix]] + find_best_scale_factor(x)
  best_prefix <- names(prefixes)[which(prefixes == best_scale_factor)[1]]
  best_prefix <- if (best_prefix == "NONE") "" else best_prefix
  x_num[x_num_idx] <- paste0(best_prefix, auto_scale_unit)

  # best prefix units
  scaled_units <- c(sprintf("(%s)", x_num), sprintf("(%s)-1", units(x)$denominator)) |>
    paste(collapse = " ")
  return(scaled_units)
}

# find the scale factor that would bring x in the range between >=1 and < 10^permissible_orders_of_mag
# @return log10 of the scale factor
find_best_scale_factor <- function(x, permissible_orders_of_mag = 3) {
  stopifnot("`x` has to be a numeric vector" = is.numeric(x))
  x <- as.numeric(x) # drop attributes
  x <- x[!is.na(x) & !is.infinite(x) & abs(x) > .Machine$double.eps] # drop NAs, Inf, and 0s
  if (length(x) == 0) {
    return(0)
  } # nothing left to scale with --> no scaling
  # find best factor scaling for the median of |x|
  best <- (log10(stats::median(abs(x))) %/% permissible_orders_of_mag) *
    permissible_orders_of_mag
  return(best)
}
