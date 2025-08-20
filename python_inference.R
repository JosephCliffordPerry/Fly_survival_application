#script to run a python inference 
library(reticulate)

# ✅ Use ephemeral Python env
Sys.setenv(RETICULATE_PYTHON = "managed")

# ✅ Only need ultralytics + numpy now
py_require(
  packages = c(
    "ultralytics",
    "numpy"
  ),
  python_version = "3.12.4"
)

# Import Python modules
np <- import("numpy")
ultralytics <- import("ultralytics")

run_inference <- function(model_path, image_dir, stats_file) {
  
  # Load YOLO model
  model <- ultralytics$YOLO(model_path)
  
  # Open output file
  f <- file(stats_file, open = "w")
  writeLines("frame\tshape_id\tx1\ty1\tx2\ty2\tx3\ty3\tx4\ty4", f)
  
  # List all .tif images in R
  image_paths <- list.files(image_dir, pattern = "\\.tif$", full.names = TRUE)
  
  for (image_path in image_paths) {
    # Run inference by passing the file path directly
    results <- model$predict(
      source = image_path,
      task = "obb",
      save = FALSE,
      verbose = FALSE
    )[[1]]
    
    obb_list <- results$obb$xyxyxyxy
    obb_list_size <- obb_list$size(0L)
    if (obb_list_size > 0) {
      for (i in 0:(obb_list_size - 1)) {
        obb <- obb_list[i]$cpu()$numpy()
        
        # Convert to R matrix
        points <- matrix(unlist(py_to_r(obb)), ncol = 2, byrow = TRUE)
        
        coords <- paste(
          apply(points, 1, function(row) paste(row, collapse = "\t")),
          collapse = "\t"
        )
        
        writeLines(sprintf("%s\t%d\t%s",
                           basename(image_path),
                           i + 1,  # YOLO shape IDs start at 1
                           coords), f)
      }
    }
  }
  
  close(f)
  message(sprintf("✅ Inference complete. Stats saved to: %s", stats_file))
}

# 🔹 Example usage
run_inference(
  model_path = "D:/Fly_annotation/produced_models/Optimised_obbox3/weights/best.pt",
  image_dir = "D:/Fly_annotation/Joes_annotations/Hcr_0_50mM_3_frames",
  stats_file = "D:/Fly_annotation/Pupariation_pipeline/reticulate_test.txt"
)
