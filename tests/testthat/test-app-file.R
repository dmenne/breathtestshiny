context("breathtestshiny-app")
# This file is for testing the applications in the inst/ directory.
library(shinytest)
#library(V8) # required on travis, but does not work there because of different results
# in lower decimals

test_that("default_demo_test works", {
  appdir <- system.file(package = "breathtestshiny", "shiny")
  expect_pass(testApp(appdir, compareImages = FALSE, quiet = FALSE))
})
