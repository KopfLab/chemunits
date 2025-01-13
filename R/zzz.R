# nocov start
.onLoad <- function(libname, pkgname) {
  # install non-standard units
  units::install_unit("M", "mol/L", "molarity")
}
# nocov end
