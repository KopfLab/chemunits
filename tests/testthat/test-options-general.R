test_that("safety checks work", {
  expect_error(define_pkg_option(check_fn = 42), "must be a function")
  expect_error(get_pkg_option("na", "pkg"), "not defined")
})

test_that("functionality works", {
  expect_equal(define_pkg_option(42, is_integerish), 
               list(default = 42, check_fn_quo = quo(is_integerish)))
  
})
