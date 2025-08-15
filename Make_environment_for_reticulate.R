# create_env.R
library(reticulate)

# Configuration
env_name <- "r-yolo_env"
miniconda_path <- normalizePath("env", mustWork = FALSE)
env_dir <- file.path(miniconda_path, env_name)  # ./env/yolo_env
is_windows <- .Platform$OS.type == "windows"

# 1️⃣ Install Miniconda locally if missing
try({
if (!dir.exists(miniconda_path)) {
  message("📦 Installing Miniconda locally in ./env ...")
  install_miniconda(path = miniconda_path, update = FALSE)
} else {
  message("✅ Miniconda already present.")
}
})
?install_python
# 2️⃣ Path to local conda binary
conda_bin <- if (is_windows) {
  file.path(miniconda_path, "condabin", "conda.bat")
} else {
  file.path(miniconda_path, "bin", "conda")
}
conda_bin <- normalizePath(conda_bin)

# 3️⃣ Create self-contained environment (only in ./env/yolo_env)
if (!dir.exists(env_dir)) {
  message("🛠 Creating local conda environment: ", env_name)
 conda_create(envname = env_dir, python_version = "3.12.4")
} else {
  message("✅ Environment '", env_name, "' already exists at ", env_dir)
}

