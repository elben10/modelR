context("test-mod_lm.R")

test_that("mod_lm returns list of class mod_lm", {
  testthat::expect_s3_class(mod_lm(mtcars, mpg~cyl), "mod_lm")
})
