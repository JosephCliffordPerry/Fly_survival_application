Avi_to_tiff <- function(video_folder, log_fun = NULL) {
  ffmpeg_path <- get_ffmpeg_path()
  if (!file.exists(ffmpeg_path)) stop("FFmpeg not found at: ", ffmpeg_path)

  avi_files <- list.files(video_folder, pattern = "\\.avi$", full.names = TRUE, ignore.case = TRUE)
  if (length(avi_files) == 0) {
    if (!is.null(log_fun)) log_fun("No AVI files found in folder")
    return()
  }

  shiny::withProgress(message = "Converting AVI -> TIFF...", value = 0, {

    n_files <- length(avi_files)

    for (i in seq_along(avi_files)) {
      avi_file <- avi_files[i]
      base_name <- tools::file_path_sans_ext(basename(avi_file))
      output_folder <- file.path("avi_frames", paste0(base_name, "_frames"))
      dir.create(output_folder, showWarnings = FALSE)
      output_pattern <- file.path(output_folder, paste0(base_name, "_%04d.tif"))

      # Run ffmpeg and capture exit status
      exit_status <- system2(
        ffmpeg_path,
        args = c("-i", shQuote(avi_file), "-pix_fmt", "rgb24", "-color_range", "pc", shQuote(output_pattern))
      )

      msg <- if (exit_status == 0) {
        paste("Finished converting:", basename(avi_file))
      } else {
        paste("Failed to convert:", basename(avi_file))
      }

      if (!is.null(log_fun)) log_fun(msg)

      # update progress bar
      shiny::setProgress(value = i / n_files, detail = paste("Processing", basename(avi_file)))
    }
  })
}
