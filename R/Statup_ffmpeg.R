get_ffmpeg_path <- function() {
  # ---- Get system info ----
  sysname <- Sys.info()[["sysname"]]

  # ---- Check system ffmpeg ----
  sys_ffmpeg <- Sys.which("ffmpeg")
  if (nzchar(sys_ffmpeg)) {
    append_msg(paste("Found system ffmpeg:", sys_ffmpeg))
    return(normalizePath(sys_ffmpeg))
  }

  append_msg("FFmpeg not found. Using managed Python environment...")

  # ---- Force managed Python ----
  base::Sys.setenv(RETICULATE_PYTHON = "managed")

  # ---- Try installing/ensuring required Python environment ----
  tryCatch({
    # Capture Python setup output
    base::withCallingHandlers({

      reticulate::py_require(
        packages = c("imageio-ffmpeg", "ultralytics", "numpy"),
        python_version = "3.12.4"
      )

      append_msg(output <- capture.output({
        py_run_string("print('Python setup via reticulate ran')")
      }))

      output_text <- paste(output, collapse = "\n")
      print(output_text)

      ffmpeg <- reticulate::import("imageio_ffmpeg")
      exe <- ffmpeg$get_ffmpeg_exe()

      append_msg(paste("FFmpeg executable found at:", exe))
      return(base::normalizePath(exe))

    },
    message = function(m) {
      append_msg(base::paste("Python message:", m$message))
    },
    warning = function(w) {
      append_msg(base::paste("Python warning:", w$message))
    })

  }, error = function(e) {
    append_msg(base::paste("Error initializing Python FFmpeg environment:", e$message))
    stop("Failed to initialize FFmpeg through Python.")
  })
}

append_msg <- function(msg) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  full_msg <- paste0(timestamp, " ", msg, "\n")

  # Print to console (for R console debugging)
  base::cat(full_msg)

  # Try appending to the session temp log and permanent log
  log_dir <- "flySurvivalApp_output"
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  permanent_log <- file.path(log_dir, "flySurvivalApp_log.txt")

  try({
    # Append to temp log (if sink file exists)
    temp_files <- list.files(tempdir(), pattern = "^flyOut_.*\\.txt$", full.names = TRUE)
    if (length(temp_files) > 0 && file.exists(temp_files[1])) {
      cat(full_msg, file = temp_files[1], append = TRUE)
    }

    # Always append to permanent log
    cat(full_msg, file = permanent_log, append = TRUE)
  }, silent = TRUE)
}

