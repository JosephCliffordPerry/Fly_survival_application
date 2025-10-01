library(testthat)
library(withr)
library(shiny)

test_that("Avi_to_tiff handles empty folder gracefully", {
  tmp <- local_tempdir()

  log_messages <- character()
  log_fun <- function(msg) log_messages <<- c(log_messages, msg)

  # Should not error and should log "No AVI files found"
  expect_silent(Avi_to_tiff(tmp, log_fun = log_fun))
  expect_true(any(grepl("No AVI files found", log_messages)))
})


