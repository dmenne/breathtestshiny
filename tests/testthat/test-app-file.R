context("breathtestshiny-app")
# This file is for testing the applications in the inst/ directory.
library(shinytest)
#library(V8) ä required on travis, but does not work there because of different results
# in lower decimals

test_that("default_demo_test works", {
  skip_on_cran()
  skip_on_travis()
  appdir <- system.file(package = "breathtestshiny", "shiny")
  expect_pass(testApp(appdir, compareImages = FALSE, quiet = FALSE))
})
