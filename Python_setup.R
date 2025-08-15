system2("Rscript", args = "Make_environment_for_reticulate.R")
library(reticulate)
# Configuration
env_name <- "yolo_env"
miniconda_path <- normalizePath("env", mustWork = FALSE)
env_dir <- file.path(miniconda_path, env_name)  # ./env/yolo_env
is_windows <- .Platform$OS.type == "windows"
# 2️⃣ Path to local conda binary
conda_bin <- if (is_windows) {
  file.path(miniconda_path, "condabin", "conda.bat")
} else {
  file.path(miniconda_path, "bin", "conda")
}
conda_bin <- normalizePath(conda_bin)

use_condaenv(env_dir, conda = conda_bin, required = TRUE)
