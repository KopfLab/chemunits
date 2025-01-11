#' automatically scale numerators to the best udunits prefix (`units` would be an option in units_options)
#' @export
auto_scale_units <- function(x, units = c("mol", "M", "L")) {
  
  # safety check
  stopifnot(
    "`x` has to have units" = !missing(x) && is(x, "units"),
    "`units` has to be a scalar" = 
      rlang::is_empty(units) || rlang::is_character(units)
  )
  if (rlang::is_empty(units)) return(x)
  
  # SI prefixes (could get these from units::valid_udunits_prefixes() but the xml dependency just for that seems silly)
  prefixes <- c(
    "y" = -24, "z" = -21, "a" = -18, "f" = -15, 
    "p" = -12, "n" = -9, "\U00B5" = -6, "u" = -6, "m" = -3, 
    "NONE" = 0, "k" = 3, "M" = 6, "G" = 9, "T" = 12, 
    "P" = 15, "E" = 18, "Z" = 21, "Y" = 24)
  
  # which units apply?
  x_num <- base::units(x)$numerator
  if (length(x_num) == 0L) return(x) # no numerator
  auto_scale <- lapply(units, function(u) grepl(paste0(u, "$"), x_num))
  auto_scale_matches <- sapply(auto_scale, sum)
  
  if(sum(auto_scale_matches) == 0L) return(x) # none of the units in denominator
  if (sum(auto_scale_matches) > 1L) { # more than one of the auto-scale units in denominator
    sprintf("more than one valid auto-scale unit for denominator '%s' - will NOT autoscale",
            paste(x_num, collapse = "*")) |>
      warning(immediate. = TRUE, call. = FALSE)
    return(x)
  }
  
  # get current prefix
  auto_scale_unit <- units[auto_scale_matches == 1]
  x_num_idx <- which(auto_scale[auto_scale_matches == 1][[1]])
  current_prefix <- sub(paste0(auto_scale_unit, "$"), "", x_num[x_num_idx])
  current_prefix <- if(current_prefix == "") "NONE" else current_prefix
  if (!current_prefix %in% names(prefixes)) { # should be impossible but just for safety
    sprintf("not a known metric prefix '%s' in numerator unit '%s' - will NOT autoscale",
            current_prefix, x_num[x_num_idx]) |>
      warning(immediate. = TRUE, call. = FALSE)
    return(x)
  }
  
  # get best prefix for auto-scaling
  best_scale_factor <- prefixes[[current_prefix]] + find_best_scale_factor(x)
  best_prefix <- names(prefixes)[which(prefixes == best_scale_factor)[1]]
  best_prefix <- if(best_prefix == "NONE") "" else best_prefix
  x_num[x_num_idx] <- paste0(best_prefix, auto_scale_unit)
  
  # scale to the best prefix
  scaled_units <- c(sprintf("(%s)", x_num), sprintf("(%s)-1", base::units(x)$denominator)) |> 
    paste(collapse = " ")
  x_scaled <- units::set_units(x, scaled_units, mode = "standard", implicit_exponents = T)
  return(x_scaled)
}

# find the scale factor that would bring x in the range between >=1 and < 10^permissible_orders_of_mag
# @return log10 of the scale factor
find_best_scale_factor <- function(x, permissible_orders_of_mag = 3) {
  stopifnot("`x` has to be a numeric vector" = is.numeric(x))
  x <- as.numeric(x) # drop attributes
  x <- x[!is.na(x) & !is.infinite(x) & abs(x) > .Machine$double.eps] # drop NAs, Inf, and 0s
  if (length(x) == 0) return(0) # nothing left to scale with --> no scaling
  # find best factor scaling for the median of |x|
  best <- (log10(stats::median(abs(x))) %/% permissible_orders_of_mag) *
    permissible_orders_of_mag
  return(best)
}