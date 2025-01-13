test_that("safety checks work", {
  # chemunits_options
  # --> all tested in test-options-general.R
  
  # get_chemunits_option
  # --> all tested in test-options-general.R
  
  # get_pkg_options
  # --> all tested in test-options-general.R
  
  # do_units_exist
  expect_error(do_units_exist(42), "must be.*character vector")
  expect_error(do_units_exist("DNE"), "not recognized")
})

# test_that("functionality works", {
#   # chemunits_options
#   
#   # FIXME: continue here
# })