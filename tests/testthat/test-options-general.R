test_that("safety checks work", {
  # define_pkg_option
  expect_error(define_pkg_option(check_fn = 42), "must be a function")
  expect_snapshot(
    error = TRUE, 
    define_pkg_option(default = 42L, check_fn = function(x) stop("issue"))
  )
  expect_snapshot(
    error = TRUE, 
    define_pkg_option(default = 42L, check_fn = function(x) return("not T/F"))
  )
  expect_snapshot(
    error = TRUE, 
    define_pkg_option(default = 42L, check_fn = function(x) return(FALSE))
  )
  
  # get_pkg_option
  expect_error(get_pkg_option("dne", "pkg"), "not defined")
  
  # set_pkg_option
  expect_error(set_pkg_option("dne", "value", "pkg"), "not defined")
})

test_that("functionality works", {
  # define_pkg_option
  expect_equal(define_pkg_option(42, is_integerish), 
               list(default = 42, check_fn_quo = quo(is_integerish)))
  
  
})
