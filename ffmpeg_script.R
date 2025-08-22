Avi_to_tiff <- function(video_folder) {
  # Detect OS and set path to bundled ffmpeg
  get_ffmpeg_path <- function() {
    sysname <- Sys.info()[["sysname"]]
    
    app_dir <- normalizePath("Fly_survival_application", mustWork = TRUE)
    
    if (sysname == "Windows") {
      return(file.path(app_dir, "ffmpeg_windows", "ffmpeg.exe"))
    } else if (sysname == "Darwin") {
      return(file.path(app_dir, "ffmpeg_mac", "ffmpeg"))
    } else {
      stop("Unsupported OS for bundled ffmpeg")
    }
  }
  
  # Path to ffmpeg
  ffmpeg_path <- get_ffmpeg_path()
  
  avi_files <- list.files(video_folder, pattern = "\\.AVI$", full.names = TRUE)
  
  for (avi_file in avi_files) {
    base_name <- tools::file_path_sans_ext(basename(avi_file))
    output_folder <- file.path(video_folder, paste0(base_name, "_frames"))
    dir.create(output_folder, showWarnings = FALSE)
    output_pattern <- file.path(output_folder, paste0(base_name, "_%04d.tif"))
    
    # Use explicit pixel format and full color range
    cmd <- sprintf('"%s" -i "%s" -pix_fmt rgb24 -color_range pc "%s"',
                   ffmpeg_path, avi_file, output_pattern)
    
    system(cmd)
  }
}