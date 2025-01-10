#  chemunits class --------

# turn `units` into `chemunits` --> 
# FIXME this should be more of a conversion class --> RENAME IT + PARAMETERS
add_chemunits_class <- function(x) {
  stopifnot("`x` must be a units object" = !missing(x) && is(x, "units"))
  class(x) <- c("chemunits", class(x))
  return(x)
}

#' `chemunits` vector
#'
#' @param x A numeric vector
#' @return An S3 vector of class `chemunits`.
#' @export
#' @examples
#' set_chemunits(c(0.25, 0.5, 0.75))
set_chemunits <- function(x, value, ...) {
  x |> 
    # use quo and tidy eval to accommodate both standard and NSE
    units::set_units(!!enexpr(value), !!!enquos(...)) |>
    quo() |> eval_tidy() |>
    add_chemunits_class()
}

#' @describeIn set_chemunits short-form for the `set_chemunits` function
#' @aliases set_cu
#' @export
set_cu <- set_chemunits

# for compatibility with the S4 system
methods::setOldClass(c("chemunits", "vctrs_vctr"))

# don't think we need this
# #' @export
# as_chemunits <- function(x, ...) {
#   UseMethod("as_chemunits")
# }
# 
# #' @export
# as_chemunits.default <- function(x, ...) {
#   units::as_units(x, ...) |> add_chemunits_class()
# }
# 
# #' @export
# as_chemunits.units <- function(x, ...) {
#   units::as_units(x, ...) |> add_chemunits_class()
# }

# S3 units::as_units/drop_units --------

#' @importFrom units as_units
#' @export
units::as_units

#' @export
as_units.chemunits <- function(x) {
  class(x) <- setdiff(class(x), "chemunits")
  NextMethod()
}

#' @importFrom units drop_units
#' @export
units::drop_units

#' @export
drop_units.chemunits <- function(x) {
  class(x) <- setdiff(class(x), "chemunits")
  NextMethod()
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
rep.chemunits <- function(x, ...) { NextMethod() |> add_chemunits_class() }

#' @export
diff.chemunits <- function(x, ...) { NextMethod() |> add_chemunits_class() }

# S3 base::Math/ ---------

#' @export
Math.chemunits <- function(x, ...) {
  x <- NextMethod()
  if(is(x, "units")) return(add_chemunits_class(x))
  return(x)
}

#' @export
Ops.chemunits <- function(e1, e2) {
  x <- NextMethod()
  # FIXME: check if units changed and decide what to do with it from then
  x |> add_chemunits_class()
}

# S3 base::min/max/sum/range/mean/median/quantile / stats::weighted.mean ----------

# Summary = min/max/sum/range
#' @export
Summary.chemunits <- function(..., na.rm = FALSE) { NextMethod() |> add_chemunits_class() }
#' @export
mean.chemunits <- function(x, ...) { NextMethod() |> add_chemunits_class() }
#' @export
median.chemunits <- function(x, ...) { NextMethod() |> add_chemunits_class() }
#' @export
weighted.mean.chemunits <- function(x, w, ...) { NextMethod() |> add_chemunits_class() }
#' @export
quantile.chemunits <- function(x, ...) { NextMethod() |> add_chemunits_class() }


# S3 base::c / base::as.list -------

#' @export
c.chemunits <- function(...) {
  old <- units::units_options(allow_mixed = FALSE)
  on.exit(units::units_options(old))
  # don't allow mixed chemunits for now (maybe ever), they may cause more trouble than it's worth
  tryCatch(
    NextMethod() |> add_chemunits_class(),
    error = function(e) {
      local_error_call(sys.call(1))
      if (grepl("cannot be mixed", conditionMessage(e))) {
        abort("mixed chemunits are not supported, units::mixed_units() may provide the functionality you are looking for")
      } else {
        abort(conditionMessage(e))
      }
    }
  )
}

#' @export
as.list.chemunits <- function(x, ...) {
  NextMethod() |> lapply(add_chemunits_class)
}

# S3 vctrs::vec_proxy/vec_restore/vec_ptype2/vec_cast -----------

# is the proxy stuff necessary? seems to be

#' @export
vec_proxy.chemunits = function(x, ...) x

#' @export
vec_restore.chemunits = function(x, to, ...) {
  units:::restore_units(x, to) |> add_chemunits_class()
}

#' @export
vec_ptype2.chemunits.chemunits <- function(x, y, ...) {
  vctrs::vec_ptype2(units::as_units(x), units::as_units(y)) |>
    add_chemunits_class()
}

# if one is chemunits augment both to chemunits
#' @export
vec_ptype2.chemunits.units <- function(x, y, ...) {
  vctrs::vec_ptype2(units::as_units(x), y) |>
    add_chemunits_class()
}

#' @export
vec_ptype2.units.chemunits <- function(x, y, ...) {
  vctrs::vec_ptype2(x, units::as_units(y)) |>
    add_chemunits_class()
}

#' @export
vec_cast.chemunits.chemunits <- function(x, to, ...) {
  vctrs::vec_cast(units::as_units(x), units::as_units(to)) |>
    add_chemunits_class()
}

# arithmetic ---------

#' #' @export
#' #' @method vec_arith chemunits
#' vec_arith.chemunits <- function(op, x, y, ...) {
#'   UseMethod("vec_arith.chemunits", y)
#' }
#' 
#' #' @export
#' #' @method vec_arith.chemunits chemunits
#' vec_arith.chemunits.chemunits <- function(op, x, y, ...) {
#'   # implementation here
#' }
