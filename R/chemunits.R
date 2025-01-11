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
set_chemunits <- function(x, value, ..., convert_to_default = FALSE, default_units = c(), auto_scale = TRUE, auto_scale_units = c()) {
  x |> 
    # use quo and tidy eval to accommodate both standard and NSE
    units::set_units(!!enexpr(value), !!!enquos(...)) |>
    quo() |> eval_tidy() |>
    as_chemunits()
}

#' @describeIn set_chemunits short-form for the `set_chemunits` function
#' @aliases set_cu
#' @export
set_cu <- set_chemunits

#' convert to chemunits
#' @keywords internal
as_chemunits <- function(x = double(), ..., convert_to_default = FALSE, default_units = c(), auto_scale = TRUE, auto_scale_units = c()) {
  
  sprintf("runnning `as_chemunits` on %s (%s) with convert_to_default = %s, auto_scale = %s",
          paste(as.numeric(x), collapse = ", "),
          if(is(x, "units")) units(x) |> as.character() else "NA",
          if(convert_to_default) "TRUE" else "FALSE",
          if (auto_scale) "TRUE" else "FALSE") |> print()
  
  # check if already `chemunits` and neither converting to default nor auto-scaling
  if(is(x, "chemunits") && !convert_to_default && !auto_scale) 
    return(x)
  
  # if not `units`, convert to units and add `chemunits` class
  if(!is(x, "units")) x <- units::as_units(x, ...)
  class(x) <- c("chemunits", class(x))
  
  # # convert to default units
  # if (convert_to_default)
  #   x <- convert_to_default_units(x, default_units)
  # 
  # # autoscale units
  # if (auto_scale)
  #   x <- auto_scale_units(x, auto_scale_units)
  
  return(x)
}

#' `chemunits` vector
#'
#' @param x a chemvalue vector
#' @return A `double` vector
#' @export
get_chemvalue <- function(cu, units, ...) {
  
}

#' @export
get_cv <- get_chemvalue

# S3 vctrs::vec_proxy/vec_restore/vec_ptype2/vec_cast -----------

#' @export
vec_proxy.chemunits = function(x, ...) x

#' @export
vec_restore.chemunits = function(x, to, ...) {
  units:::restore_units(x, to) |> as_chemunits()
}

#' @export
vec_ptype2.chemunits.chemunits <- function(x, y, ...) {
  vctrs::vec_ptype2(units::as_units(x), units::as_units(y)) |>
    as_chemunits()
}

#' @export
vec_cast.chemunits.chemunits <- function(x, to, ...) {
  vctrs::vec_cast(units::as_units(x), units::as_units(to)) |>
    as_chemunits()
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
  NextMethod() |> as_chemunits(convert_to_default = FALSE, auto_scale = FALSE)
}

# S3 base::c / base::as.list -------

#' @export
c.chemunits <- function(...) {
  old <- units::units_options(allow_mixed = FALSE)
  on.exit(units::units_options(old))
  # don't allow mixed chemunits for now (maybe ever)
  try_fetch(
    NextMethod() |> as_chemunits(),
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
  NextMethod() |> lapply(as_chemunits)
}

# S3 base::print/format -----------

#' @export
print.chemunits <- function (x, ...) { 
  if (is.array(x) || length(x) > 1L) cat("Chem")
  NextMethod() 
}

#' @export
format.chemunits <- function(x, ...) { NextMethod() }

# S3 base::rep/diff ---------

#' @export
rep.chemunits <- function(x, ...) { NextMethod() |> as_chemunits() }

#' @export
diff.chemunits <- function(x, ...) { NextMethod() |> as_chemunits() }

# S3 base::min/max/sum/range/mean/median/quantile/Math/Ops / stats::weighted.mean ----------

# Summary = min/max/sum/range
#' @export
Summary.chemunits <- function(..., na.rm = FALSE) { NextMethod() |> as_chemunits() }
#' @export
mean.chemunits <- function(x, ...) { NextMethod() |> as_chemunits() }
#' @export
median.chemunits <- function(x, ...) { NextMethod() |> as_chemunits() }
#' @export
weighted.mean.chemunits <- function(x, w, ...) { NextMethod() |> as_chemunits() }
#' @export
quantile.chemunits <- function(x, ...) { NextMethod() |> as_chemunits() }

# Math = abs/sign/floor/etc
#' @export
Math.chemunits <- function(x, ...) {
  x <- NextMethod()
  if(is(x, "units")) return(as_chemunits(x))
  return(x)
}

# Ops = comparisons (>, <, ==, etc.) + arithmetic (+, -, *, /, etc.)
#' @export
Ops.chemunits <- function(e1, e2) {
  x <- NextMethod()
  # check if result has units
  if (is(x, "units")) {
    e1_same_units <- !is(e1, "units") || identical(units(x), units(e1))
    e2_same_units <- !is(e2, "units") || identical(units(x), units(e2))
    if (e1_same_units && e2_same_units)
      # units have not change
      x <- as_chemunits(x, convert_to_default = FALSE, auto_scale = TRUE)
    else 
      # units have changed --> convert to default
      x <- as_chemunits(x, convert_to_default = TRUE, auto_scale = TRUE)  
  }
  return(x)
}
