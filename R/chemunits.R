#  chemunits class --------

# for compatibility with the S4 system
methods::setOldClass(c("chemunits", "vctrs_vctr"))

#' `chemunits` vector
#'
#' @param x A numeric vector
#' @return An S3 vector of class `chemunits`.
#' @export
#' @examples
#' set_chemunits(c(0.25, 0.5, 0.75))
set_chemunits <- function(x, units, ..., convert_to_default = FALSE, auto_scale = TRUE) {
  x |>
    # use quo and tidy eval to accommodate both standard and NSE
    units::set_units(!!enexpr(units), !!!enquos(...)) |>
    quo() |>
    eval_tidy() |>
    as_chemunits(
      convert_to_default = convert_to_default,
      auto_scale = auto_scale
    )
}

#' @describeIn set_chemunits short-form for the `set_chemunits` function
#' @aliases set_cu
#' @export
set_cu <- set_chemunits

#' convert to chemunits
#' @keywords internal
as_chemunits <- function(x = double(), ..., convert_to_default = FALSE, auto_scale = TRUE) {
  # if not `units`, convert to units and add `chemunits` class
  if (!is(x, "units")) x <- units::as_units(x, ...)
  x <- add_chemunits_class(x)

  # convert to default units
  if (convert_to_default) x <- convert_to_default_units(x)

  # autoscale units
  if (auto_scale) x <- auto_scale_units(x)

  return(x)
}

# convenience function
add_chemunits_class <- function(x) {
  stopifnot(is(x, "units"))
  if (!is(x, "chemunits")) class(x) <- c("chemunits", class(x))
  return(x)
}

#' `chemunits` vector
#'
#' @param cu a chemunits vector
#' @return A `double` vector
#' @export
get_chemvalue <- function(cu, units, ...) {
  check_required(cu)
  if (missing(units)) {
    cli_abort(
      "{.var units} must be specified when retrieving a chemvalue to ensure
      the numbers you are working with afterwards are exactly in the units
      you expect them to be",
      "i" = "while using {.var as.numeric()} works, it is not recommended"
    )
  }
}

#' @export
get_cv <- get_chemvalue

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
as_units.chemunits <- function(x) {
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
format.chemunits <- function(x, ...) {
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
#' @export
median.chemunits <- function(x, ...) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}
#' @export
weighted.mean.chemunits <- function(x, w, ...) {
  NextMethod() |>
    add_chemunits_class() |>
    auto_scale_units()
}
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
        convert_to_default_units() |>
        auto_scale_units()
    }
  }
  return(x)
}
