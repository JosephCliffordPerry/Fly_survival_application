# === Helper Functions (pure R for IoU) ===
obb_iou <- function(obb1, obb2) {
  # obb1/obb2 are numeric matrices with shape (4,2)
  poly1 <- st_polygon(list(rbind(obb1, obb1[1, ]))) |> st_sfc()
  poly2 <- st_polygon(list(rbind(obb2, obb2[1, ]))) |> st_sfc()
  
  inter <- suppressWarnings(st_intersection(poly1, poly2))
  if (length(inter) == 0) return(0.0)
  
  area_inter <- as.numeric(st_area(inter))
  area1 <- as.numeric(st_area(poly1))
  area2 <- as.numeric(st_area(poly2))
  union <- area1 + area2 - area_inter
  area_inter / union
}

merge_predictions <- function(preds, iou_threshold = 0.5) {
  merged <- list()
  for (obb in preds) {
    keep <- TRUE
    for (m in merged) {
      if (obb_iou(obb, m) > iou_threshold) {
        keep <- FALSE
        break
      }
    }
    if (keep) merged <- append(merged, list(obb))
  }
  merged
}

run_inference_multi <- function(model_paths, image_dir, stats_file, iou_threshold = 0.5, log_fun = message) {

  # ✅ Use ephemeral Python env S
  Sys.setenv(RETICULATE_PYTHON = "managed") 
  
  # Only need ultralytics +numpy
  py_require( packages = c( "ultralytics", "numpy" ), python_version = "3.12.4" ) 
  np<- import("numpy") 
  ultralytics <- import("ultralytics")
  # Load all models
  log_fun(sprintf("models loading"))
  models <- lapply(model_paths, function(p) ultralytics$YOLO(p))
  log_fun(sprintf("models loaded"))
  # Open output file
  f <- file(stats_file, open = "w")
  writeLines("frame\tshape_id\tx1\ty1\tx2\ty2\tx3\ty3\tx4\ty4", f)
  
  # List all .tif images
  image_paths <- list.files(image_dir, pattern = "\\.tif$", full.names = TRUE)
  n_images <- length(image_paths)
  
  for (idx in seq_along(image_paths)) {
    image_path <- image_paths[idx]
    all_preds <- list()
    
    # Log progress
    log_fun(sprintf("🖼 Processing image %d/%d: %s", idx, n_images, basename(image_path)))
    
    # Run inference for each model
    for (model in models) {
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
          points <- matrix(unlist(py_to_r(obb)), ncol = 2, byrow = TRUE)
          all_preds <- append(all_preds, list(points))
        }
      }
    }
    
    # Merge overlapping predictions
    merged_preds <- merge_predictions(all_preds, iou_threshold)
    
    # Write merged results
    for (i in seq_along(merged_preds)) {
      coords <- paste(
        apply(merged_preds[[i]], 1, function(row) paste(row, collapse = "\t")),
        collapse = "\t"
      )
      writeLines(sprintf("%s\t%d\t%s",
                         basename(image_path),
                         i,
                         coords), f)
    }
    
    # Log completion for this image
    log_fun(sprintf("✅ Done image %d/%d", idx, n_images))
  }
  
  close(f)
  log_fun(sprintf("🎉 Multi-model inference complete! Stats saved to: %s", stats_file))
}
