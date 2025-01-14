#  chemunits class --------

# for compatibility with the S4 system
methods::setOldClass(c("chemunits", "vctrs_vctr"))

#' `chemunits` vector
#'
#' @param x A numeric vector
#' @param units units
#' @param convert_to_preferred whether to convert to default (if there is default)
#' @param auto_scale whether to auto-scale (if units are scalable)
#' @return An S3 vector of class `chemunits`.
#' @export
#' @examples
#' set_chemunits(c(0.25, 0.5, 0.75))
set_chemunits <- function(x, units, convert_to_preferred = FALSE, auto_scale = TRUE) {
  x |>
    # use quo and tidy eval to accommodate both standard and NSE
    units::set_units(!!enexpr(units)) |>
    quo() |>
    eval_tidy() |>
    as_chemunits(
      convert_to_preferred = convert_to_preferred,
      auto_scale = auto_scale
    )
}

#' @describeIn set_chemunits short-form for the `set_chemunits` function
#' @aliases set_cu
#' @export
set_cu <- set_chemunits

#' convert to chemunits
#' @param ... passed on to [units::as_units()]
#' @keywords internal
as_chemunits <- function(x = double(), ..., convert_to_preferred = FALSE, auto_scale = TRUE) {
  # if not `units`, convert to units and add `chemunits` class
  if (!is(x, "units")) x <- units::as_units(x, ...)
  x <- add_chemunits_class(x)

  # convert to default units
  if (convert_to_preferred) x <- convert_to_preferred_units(x)

  # autoscale units
  if (auto_scale) x <- auto_scale_units(x)

  return(x)
}

# convenience function
add_chemunits_class <- function(x, call = caller_env()) {
  x <- 
    try_fetch(force(x), error = function(cnd) {
      cli_abort(conditionMessage(cnd), parent = NA, call = call)
    })
  stopifnot(is(x, "units"))
  if (!is(x, "chemunits")) class(x) <- c("chemunits", class(x))
  return(x)
}

#' `chemunits` vector
#'
#' [get_chemvalue()] retrieves the ... FIXME
#'
#' The alternative [drop_units()] (as well as the base function [as.numeric()]) simply returns the numeric value and drops the units. It can be useful in calculations but without specifying the output units like [get_chemvalue()] requires, it does not communicate intent as clearly and can lead to accidental mistakes such as not realizing what units a number has or dropping the units from an auto-scaling unit without realizing what SI prefix the unit has. The primary use of [drop_units()] is in data frames/tibbles where it can drop the units of all columns at once.
#'
#' @param x a chemunits vector
#' @param units the units
#' @return A `double` vector
#' @export
get_chemvalue <- function(x, units) {
  check_required(x)
  if (!is(x, "units"))
    cli_abort("{.var x} must be a units or chemunits object,
              not {.obj_type_friendly {x}}")
  if (missing(units)) {
    cli_abort(
      "{.var units} must be specified when retrieving a chemvalue to ensure
      the numbers you are working with afterwards are exactly in the units
      you expect them to be",
      "i" = "while using {.var as.numeric()} works, it is not recommended"
    )
  }
  # enforce converstion to the requested units before returning the value
  units(x) <- units
  return(as.numeric(x))
}


#' @describeIn get_chemvalue short-form for the `get_chemvalue` function
#' @export
get_cv <- get_chemvalue

#' Retrieve units
#' 
#' This function retrieves a text representation of the units of a units/chemunits object. To get the symbolic units, use [units::units()] instead.
#' 
#' @inheritParams get_chemvalue
#' @param prefix the prefix text for the units. Default: `""` (no prefix).
#' @param suffix the suffix text for the units. Default: `""` (no suffix).
#' @param group parameter to specific the prefix and suffix text as a vector of length 2. This is helpful when using .e.g the prefix/suffix from the [units::units_options()] `group` for this: `group = units_options("group")`.
#' @return a text representation of the units
#' @examples
#' set_cu(1, "m/s") |> get_units()
#' set_cu(1, "m/s") |> get_units(prefix = "(", suffix = ")")
#' set_cu(1, "m/s") |> get_units(group = units_options("group"))
#' @export
get_units <- function(x, prefix = group[1], suffix = group[2], group = c("", "")) {
  stopifnot("`group` must be a character vector of length 2" = 
              is_character(group) && length(group) == 2L)
  stopifnot("`prefix` must be a string" = is_scalar_character(prefix))
  stopifnot("`suffix` must be a string" = is_scalar_character(suffix))
  if (is(x, "units"))
    return(paste0(prefix, as.character(units(x)), suffix))
  else if (is.list(x))
    return(sapply(x, get_units))
  else
    return(NA_character_)
}

#' Make units explicit
#'
#' This function is intended for data frames / tibbles only and makes the specified columns explicit with the target units in the column name.
#'
#' @param df the data frame in which to make the units explicit
#' @param ... which columns to make explicit ([dplyr::select())] syntax). If none provided, makes all columns explicit.
#' @param prefix the prefix text for the units. Default: `" ["` (based on [units::units_options()]).
#' @param suffix the suffix text for the units. Default: `"]"` (based on [units::units_options()]).
#' @param group parameter to specify the prefix and suffix text as a vector of length 2. This is helpful because it allows compatiblity with the [units::units_options()] `group` option.
#' @return a text representation of the units
#' @family quantity functions
#' @examples
#' # data frame with quantities
#' df <- tibble(
#'   weight = set_cu(1:5, "mg"),
#'   vol = set_cu(20, "mL"),
#'   mw = set_cu(500, "g/mol"),
#'   amount = weight / mw,
#'   conc = amount / vol
#' )
#'
#' # make all units columns explicit
#' df |> make_units_explicit()
#' 
#' # change the prefix and suffix
#' df |> make_units_explicit(prefix = ".", suffix = "")
#' 
#' # make a specific subset of columns explicit (using tidy select syntax)
#' df |> make_units_explicit(weight:mw) # from weight to mw
#' df |> make_units_explicit(-mw) # all but mw
#' @export
make_units_explicit <- function(df, ..., prefix = paste0(" ", group[1]), suffix = group[2], group = units_options("group")) {
  
  # safety checks
  stopifnot(
    "`df` must be a data frame or tibble" = !missing(df) && is.data.frame(df)
  )
  if (ncol(df) == 0L) return(df) # nothing to do
  
  # columns
  cols_quos <- enquos(...)
  if (length(cols_quos) == 0) {
    cols_idx <- 1:ncol(df)
  } else {
    cols_idx <- tidyselect::eval_select(expr(c(!!!cols_quos)), df)
  }
  are_units <- sapply(df[cols_idx], is, "units")
  cols_idx <- cols_idx[are_units]
  if (length(cols_idx) == 0) return(df) # nothing to do
  
  # make units explicit
  names_units <- sapply(df[cols_idx], get_units, prefix = prefix, suffix = suffix)
  names(df)[cols_idx] <- paste0(names(df)[cols_idx], names_units)
  df[cols_idx] <- sapply(df[cols_idx], drop_units)
  return(df)
}

# S3 vctrs::vec_proxy/vec_restore/vec_ptype2/vec_cast -----------

#' @export
vec_proxy.chemunits <- function(x, ...) x

#' @export
vec_restore.chemunits <- function(x, to, ...) {
  NextMethod() |> add_chemunits_class()
}

#' @export
vec_ptype2.chemunits.chemunits <- function(x, y, ...) {
  # make error clearer when units are not interconvertible
  # note: there's no way to find auto-scale prefix (x/y don't have numbers)
  # see issue #2
  ptype2 <-
    try_fetch(
      vctrs::vec_ptype2(units::as_units(x), units::as_units(y), ...) |>
        add_chemunits_class(),
      vctrs_error = function(cnd) {
        cli_abort(
          "there was a problem trying to combine two different units:
          {.emph {as.character(units(x))}} and
          {.emph {as.character(units(y))}} are not interconvertible",
          parent = cnd
        )
      }
    )
}

#' @export
vec_cast.chemunits.chemunits <- function(x, to, ...) {
  # note: there's no way to auto-scale here, see issue #2
  vctrs::vec_cast(units::as_units(x), units::as_units(to))
}

# if one is chemunits augment both to chemunits

#' @export
vec_ptype2.chemunits.units <- vec_ptype2.chemunits.chemunits

#' @export
vec_ptype2.units.chemunits <- vec_ptype2.chemunits.chemunits

#' @export
vec_cast.chemunits.units <- vec_cast.chemunits.chemunits

#' @export
vec_cast.units.chemunits <- vec_cast.chemunits.chemunits

# S3 units::as_units/drop_units/units<- --------

#' @export
as_units.chemunits <- function(x, ...) {
  class(x) <- setdiff(class(x), "chemunits")
  NextMethod()
}

#' @export
drop_units.chemunits <- function(x) {
  class(x) <- setdiff(class(x), "chemunits")
  NextMethod()
}

#' @export
`units<-.chemunits` <- function(x, value) {
  NextMethod() |> add_chemunits_class()
}

# S3 base::c / base::as.list / base::as.data.frame -------

#' @export
c.chemunits <- function(...) {
  old <- units::units_options(allow_mixed = FALSE)
  on.exit(units::units_options(old))
  # don't allow mixed chemunits for now (maybe ever)
  try_fetch(
    NextMethod() |> add_chemunits_class() |> auto_scale_units(),
    error = function(e, call = caller_env()) {
      if (grepl("cannot be mixed", conditionMessage(e))) {
        cli_abort(
          c(
            "mixed chemunits are not supported",
            "i" = "{.fn units::mixed_units} may provide the functionality you are looking for"
          ),
          parent = e
        )
      } else {
        cli_abort("something went wrong combining chemunits", parent = e)
      }
    }
  )
}

#' @export
as.list.chemunits <- function(x, ...) {
  # autoscale when splitting into a list
  NextMethod() |>
    lapply(add_chemunits_class) |>
    lapply(auto_scale_units)
}

#' @export
as.data.frame.chemunits <- function(x, ...) {
  df <- NextMethod()
  df[[1]] <- df[[1]] |> add_chemunits_class()
  return(df)
}

# S3 base::print/format -----------

#' @export
print.chemunits <- function(x, ...) {
  if (is.array(x) || length(x) > 1L) cat("Chem")
  NextMethod()
}

#' @export
format.chemunits <- function(x, ..., units, auto_scale = TRUE) {
  stopifnot("`auto_scale` must be TRUE or FALSE" = is_scalar_logical(auto_scale))
  if(missing(units) && auto_scale && !is.null(find_scalable_units(x))) {
    return(sapply(x, function(x) auto_scale_units(x) |> format(..., auto_scale = FALSE)))
  }
  if (!missing(units)) units(x) <- units
  NextMethod()
}

# S3 base::rep/diff ---------

#' @export
rep.chemunits <- function(x, ...) {
  NextMethod() |> add_chemunits_class()
}

#' @export
diff.chemunits <- function(x, ...) {
  # autoscale after taking the difference
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}

# S3 base::min/max/sum/range/mean/Math/Ops / stats::median/quantile/weighted.mean ----------

# auto scale after all these operations
# Summary = min/max/sum/range
#' @export
Summary.chemunits <- function(..., na.rm = FALSE) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}
#' @export
mean.chemunits <- function(x, ...) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}
#' @importFrom stats median
#' @export
median.chemunits <- function(x, ...) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}
#' @importFrom stats weighted.mean
#' @export
weighted.mean.chemunits <- function(x, w, ...) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}
#' @importFrom stats quantile
#' @export
quantile.chemunits <- function(x, ...) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}

# Math = abs/sign/floor/etc
#' @export
Math.chemunits <- function(x, ...) {
  x <- NextMethod()
  if (is(x, "units")) {
    x <- x |>
      add_chemunits_class() |>
      auto_scale_units()
  }
  return(x)
}

# Ops = comparisons (>, <, ==, etc.) + arithmetic (+, -, *, /, etc.)
#' @export
Ops.chemunits <- function(e1, e2) {
  x <- NextMethod()
  if (is(x, "units")) {
    e1_same_units <- !is(e1, "units") || identical(units(x), units(e1))
    e2_same_units <- !is(e2, "units") || identical(units(x), units(e2))
    if (e1_same_units && e2_same_units) {
      # units have not changed --> autoscale only
      x <- x |>
        add_chemunits_class() |>
        auto_scale_units()
    } else {
      # units have changed --> convert to default units
      x <- x |>
        add_chemunits_class() |>
        convert_to_preferred_units() |>
        auto_scale_units()
    }
  }
  return(x)
}
