# Set the folder containing .avi files
video_folder <- "C:/Users/jp19193/Box/Terrarium_paper/AVI_files/20250806-Hcr-0-50mM"
avi_files <- list.files(video_folder, pattern = "\\.AVI$", full.names = TRUE)

for (avi_file in avi_files) {
  base_name <- tools::file_path_sans_ext(basename(avi_file))
  output_folder <- file.path(video_folder, paste0(base_name, "_frames"))
  dir.create(output_folder, showWarnings = FALSE)
  output_pattern <- file.path(output_folder, paste0(base_name, "_%04d.tif"))
  
  # Use explicit pixel format and full color range
  cmd <- sprintf('ffmpeg -i "%s" -pix_fmt rgb24 -color_range pc "%s"', avi_file, output_pattern)
  system(cmd)
}

