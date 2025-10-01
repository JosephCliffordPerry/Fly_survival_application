# library(testthat)
# library(shiny)
# library(withr)


test_that("Load_fly_app returns a Shiny app object", {
  app <- Load_fly_app()

  expect_s3_class(app, "shiny.appobj")
})

test_that("Modules have ui and server functions", {
  # Assuming modules are loaded correctly
  avi <- avi_to_tiff_panel()
  expect_true(all(c("ui", "server") %in% names(avi)))

  inference <- inference_panel()
  expect_true(all(c("ui", "server") %in% names(inference)))
})
