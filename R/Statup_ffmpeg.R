get_ffmpeg_path <- function() {
  sysname <- Sys.info()[["sysname"]]
  # --- Check system ffmpeg ---
  sys_ffmpeg <- Sys.which("ffmpeg")
  if (nzchar(sys_ffmpeg)) {
    return(normalizePath(sys_ffmpeg))
  }

  # --- Install via Python managed environment ---
  message("FFmpeg not found. Using managed Python environment...")

  # Force managed Python
  Sys.setenv(RETICULATE_PYTHON = "managed")

  # Ensure Python + required packages exist
  reticulate::py_require(
    packages = c("imageio-ffmpeg","ultralytics", "numpy"),
    python_version = "3.12.4" # lock Python version
  )

  ffmpeg <- reticulate::import("imageio_ffmpeg")
  exe <- ffmpeg$get_ffmpeg_exe()

  return(normalizePath(exe))
}


