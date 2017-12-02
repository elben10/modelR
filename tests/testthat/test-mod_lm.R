context("test-mod_lm.R")

# Test mod_lm
test_that("mod_lm returns list of class mod_lm", {
  expect_is(mod_lm(mtcars, mpg~cyl), "mod_lm")
  expect_type(mod_lm(mtcars, mpg~cyl), "list")
})

# Test summary of mod_lm
test_that("summary of mod_lm returns list of class summary.mod_lm", {
  expect_is(summary(mod_lm(mtcars, mpg~cyl)), "summary.mod_lm")
  expect_type(summary(mod_lm(mtcars, mpg~cyl)), "list")
})
