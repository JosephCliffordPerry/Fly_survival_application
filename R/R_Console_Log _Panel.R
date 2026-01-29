r_console_panel <- function() {

  ui <- shiny::tabPanel(
    "R Console Log",
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(
          "reload_app",
          "Reload app",
          icon = shiny::icon("rotate-right")
        )
      )
    ),
    shiny::verbatimTextOutput("r_console_out")
  )

  server <- function(input, output, session) {

    # Permanent log file
    log_dir <- "flySurvivalApp_output"
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
    permanent_log <- file.path(log_dir, "flySurvivalApp_log.txt")
    if (!file.exists(permanent_log)) file.create(permanent_log)

    # Temporary session log
    temp_out <- tempfile(pattern = "flyOut_", fileext = ".txt")
    file.create(temp_out)

    # Open connection and sink normal output
    out_con <- file(temp_out, open = "a")
    sink(out_con, type = "output", split = TRUE)

    # Reactive value for UI display
    r_console <- shiny::reactiveVal("")

    shiny::observe({
      shiny::invalidateLater(1000)
      if (file.exists(temp_out)) {
        lines <- readLines(temp_out, warn = FALSE)
        r_console(paste(lines, collapse = "\n"))
      }
    })

    output$r_console_out <- shiny::renderText({
      r_console()
    })

    # Startup message
    cat("App loaded\n")

    # ---- Reload button handler ----
    shiny::observeEvent(input$reload_app, {
      session$sendCustomMessage("reload", list())
    })

    # Cleanup on session end
    session$onSessionEnded(function() {
      try(sink(type = "output"), silent = TRUE)
      try(close(out_con), silent = TRUE)

      if (file.exists(temp_out)) {
        cat(
          readLines(temp_out, warn = FALSE),
          sep = "\n",
          file = permanent_log,
          append = TRUE
        )
      }
      file.remove(temp_out)
    })
  }

  list(ui = ui, server = server)
}
