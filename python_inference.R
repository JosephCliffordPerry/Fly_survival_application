run_inference_multi <- function(image_dir, stats_file, iou_threshold = 0.5) {
  model_paths <- list.files("models", full.names = TRUE)
  
  # --- Safe IoU function ---
  obb_iou <- function(obb1, obb2) {
    safe_poly <- function(obb) {
      tryCatch({
        st_polygon(list(rbind(obb, obb[1, ]))) |> st_sfc() |> st_make_valid()
      }, error = function(e) NULL)
    }
    
    poly1 <- safe_poly(obb1)
    poly2 <- safe_poly(obb2)
    if (is.null(poly1) || is.null(poly2)) return(0.0)
    
    inter <- suppressWarnings(st_intersection(poly1, poly2))
    if (length(inter) == 0) return(0.0)
    
    area_inter <- as.numeric(st_area(inter))
    area1 <- as.numeric(st_area(poly1))
    area2 <- as.numeric(st_area(poly2))
    union <- area1 + area2 - area_inter
    if (union <= 0) return(0.0)
    
    area_inter / union
  }
  
  # --- Merge predictions ---
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
  
  # --- Load Python env ---
  Sys.setenv(RETICULATE_PYTHON = "managed")
  py_require(packages = c("ultralytics", "numpy"), python_version = "3.12.4")
  np <- import("numpy")
  ultralytics <- import("ultralytics")
  
  models <- lapply(model_paths, function(p) ultralytics$YOLO(p))
  
  # --- File writing with safe connection ---
  f <- file(stats_file, open = "w")
  on.exit(close(f), add = TRUE)
  writeLines("frame\tshape_id\tx1\ty1\tx2\ty2\tx3\ty3\tx4\ty4", con = f)
  
  image_paths <- list.files(image_dir, pattern = "\\.tif$", full.names = TRUE)
  n_images <- length(image_paths)
  
  withProgress(message = "Running inference...", value = 0, {
    for (idx in seq_along(image_paths)) {
      image_path <- image_paths[idx]
      all_preds <- list()
      
      for (model in models) {
        results <- tryCatch({
          model$predict(
            source = image_path,
            task = "obb",
            save = FALSE,
            verbose = FALSE
          )[[1]]
        }, error = function(e) NULL)
        
        if (is.null(results)) next
        obb_list <- results$obb$xyxyxyxy
        if (obb_list$size(0L) == 0L) next
        
        for (i in 0:(obb_list$size(0L)-1)) {
          obb <- obb_list[i]$cpu()$numpy()
          points <- matrix(unlist(py_to_r(obb)), ncol=2, byrow=TRUE)
          all_preds <- append(all_preds, list(points))
        }
      }
      
      merged_preds <- merge_predictions(all_preds, iou_threshold)
      
      for (i in seq_along(merged_preds)) {
        coords <- paste(apply(merged_preds[[i]], 1, function(row) paste(row, collapse="\t")), collapse="\t")
        writeLines(sprintf("%s\t%d\t%s", basename(image_path), i, coords), con = f)
      }
      
      # update progress
      setProgress(value = idx / n_images, detail = basename(image_path))
    }
  })
}
