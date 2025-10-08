
test_that("browse_panel returns UI and server components", {
  bp <- browse_panel(df_analysis = reactiveVal(), frame_paths = reactiveVal(c("f1.tif", "f2.tif")))
  expect_type(bp, "list")
  expect_named(bp, c("ui", "server"))
  expect_true(inherits(bp$ui, "shiny.tag"))
  expect_true(is.function(bp$server))
})

test_that("UI contains key elements", {
  bp <- browse_panel(df_analysis = reactiveVal(), frame_paths = reactiveVal())
  ui <- bp$ui

  expect_true(any(grepl("Browse Inferences", as.character(ui))))
  expect_true(any(grepl("iou_threshold", as.character(ui))))
  expect_true(any(grepl("run_analysis", as.character(ui))))
  expect_true(any(grepl("frame_plot", as.character(ui))))
})
test_that("obb_iou is available inside testServer and computes IoU", {
  # minimal valid df so server starts
  df <- data.frame(
    x1=0,y1=0, x2=1,y2=0, x3=1,y3=1, x4=0,y4=1,
    frame="f_1.tif", frame_num=1, manual=FALSE,
    stringsAsFactors = FALSE
  )

  bp <- browse_panel(df_analysis = reactiveVal(df),
                     frame_paths = reactiveVal(c("frame1.tif","frame2.tif")))

  shiny::testServer(bp$server, {
    # inside testServer the server body has executed -> obb_iou exists
    srv <- get("obb_iou", envir = environment())   # <--- correct way here

    sq1 <- matrix(c(0,0, 1,0, 1,1, 0,1), ncol=2, byrow=TRUE)
    sq2 <- matrix(c(0.5,0.5, 1.5,0.5, 1.5,1.5, 0.5,1.5), ncol=2, byrow=TRUE)

    expect_gt(srv(sq1, sq2), 0)
    expect_equal(srv(sq1, sq1), 1, tolerance = 1e-6)
  })
})


test_that("obb_iou returns 0 for invalid polygons", {
  bp <- browse_panel(df_analysis = reactiveVal(), frame_paths = reactiveVal(c("f1.tif")))

  testServer(bp$server, {
    obb_iou <- get("obb_iou", envir = environment())

    bad1 <- matrix(c(0,0, 1,1, 0,0, 1,1), ncol = 2, byrow = TRUE)
    good <- matrix(c(0,0, 1,0, 1,1, 0,1), ncol = 2, byrow = TRUE)

    expect_equal(obb_iou(bad1, good), 0)
  })
})
#
# test_that("run_analysis stops on missing or invalid df_analysis", {
#   df_react <- reactiveVal(NULL)
#   frame_react <- reactiveVal(c("f1.tif"))
#
#   bp <- browse_panel(df_react, frame_react)
#
#   expect_error(
#     testServer(bp$server, {
#       session$setInputs(run_analysis = 1)
#     }),
#     regexp = "must be a non-empty data\\.frame"
#   )
# })

# test_that("run_analysis stops on missing frames", {
#   df_react <- reactiveVal(data.frame(
#     x1 = 1, y1 = 1, x2 = 2, y2 = 1, x3 = 2, y3 = 2, x4 = 1, y4 = 2, frame = "f1.tif"
#   ))
#   frame_react <- reactiveVal(NULL)
#
#   bp <- browse_panel(df_react, frame_react)
#
#   expect_error(
#     testServer(bp$server, {
#       session$setInputs(run_analysis = 1)
#     }),
#     regexp = "no frames available"
#   )
# })
#
# test_that("run_analysis stops on missing columns", {
#   df_react <- reactiveVal(data.frame(x1 = 1, y1 = 1, frame = "f1.tif"))
#   frame_react <- reactiveVal(c("f1.tif"))
#   bp <- browse_panel(df_react, frame_react)
#
#   expect_error(
#     testServer(bp$server, {
#       session$setInputs(run_analysis = 1)
#     }),
#     regexp = "missing required columns"
#   )
# })

test_that("save_path generates unique paths", {
  df_react <- reactiveVal(data.frame(
    x1 = 1, y1 = 1, x2 = 2, y2 = 1, x3 = 2, y3 = 2, x4 = 1, y4 = 2, frame = "f1.tif"
  ))
  frame_react <- reactiveVal(c("frames/f1.tif"))
  bp <- browse_panel(df_react, frame_react)

  testServer(bp$server, {
    path <- save_path()
    expect_true(grepl("processed", path))
    expect_true(grepl("statsdir", path))
  })
})

