browse_panel <- function(df_analysis, frame_paths, stats_file = NULL) {
  list(
    ui = shiny::tabPanel(
      "Browse Inferences",
      shiny::fluidPage(
        shiny::titlePanel("Browse & Analyse Frames"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::numericInput("iou_threshold", "Merging IoU Threshold:", 0.3, min = 0, max = 1, step = 0.01),
            shiny::numericInput("min_appearances", "Minimum Appearances per ID:", 2, min = 2, step = 1),
            shiny::checkboxInput("show_only_propagated", "Show Only Propagated Boxes", FALSE),
            shiny::actionButton("run_analysis", "Load/Analyse input file", class = "btn-success"),
            shiny::actionButton("save_analysis", "Save Analysis File", class = "btn-primary"),
            shiny::hr(),
            shiny::numericInput("remove_id", "ID to Remove:", value = NA, min = 1, step = 1),
            shiny::actionButton("remove_id_btn", "Remove ID", class = "btn-warning"),
            shiny::uiOutput("frame_slider_ui")
          ),
          shiny::mainPanel(
            shiny::h4("Frame Preview"),
            shiny::plotOutput("frame_plot", height = "600px", click = "frame_plot_click")
          )
        )
      )
    ),

    server = function(input, output, session) {
      `%||%` <- function(a,b) if(!is.null(a)) a else ""

      #### Frame slider ####
      output$frame_slider_ui <- shiny::renderUI({
        shiny::req(frame_paths())
        shiny::sliderInput("frame_idx", "Frame:", min = 1, max = length(frame_paths()), value = 1, step = 1, animate = TRUE)
      })

      #### IoU function ####
      obb_iou <- function(obb1, obb2) {
        safe_poly <- function(obb) {
          tryCatch({
            sf::st_polygon(list(rbind(obb, obb[1,]))) |>
              sf::st_sfc() |>
              sf::st_make_valid()
          }, error = function(e) NULL)
        }
        poly1 <- safe_poly(obb1)
        poly2 <- safe_poly(obb2)
        if(is.null(poly1) || is.null(poly2)) return(0)
        inter <- suppressWarnings(sf::st_intersection(poly1, poly2))
        if(length(inter) == 0) return(0)
        area_inter <- as.numeric(sf::st_area(inter))
        area_union <- as.numeric(sf::st_area(poly1)) + as.numeric(sf::st_area(poly2)) - area_inter
        if(area_union <= 0) return(0)
        area_inter / area_union
      }

      #### Run Analysis & propagate ####
      shiny::observeEvent(input$run_analysis, {
        inference_stats <- df_analysis()
        # Ensure we have a data.frame with rows
        if (is.null(inference_stats) || !is.data.frame(inference_stats) || nrow(inference_stats) == 0) {
          stop("run_analysis aborted: df_analysis() must be a non-empty data.frame")
        }

        # Ensure frame_paths available
        if (is.null(frame_paths()) || length(frame_paths()) == 0) {
          stop("run_analysis aborted: no frames available (frame_paths() is empty)")
        }

        # sanity: required coordinate columns exist
        required_cols <- c("x1","y1","x2","y2","x3","y3","x4","y4","frame")
        missing_cols <- setdiff(required_cols, names(inference_stats))
        if (length(missing_cols) > 0) {
          stop("run_analysis aborted: missing required columns: ", paste(missing_cols, collapse = ", "))
        }
        shiny::req(inference_stats, frame_paths())

        New_threshold <- input$iou_threshold
        if (exists("threshold") && exists("New_threshold") && threshold != New_threshold) {
          message("IOU threshold changed rerunning analysis.")
          inference_stats$id <- NA_integer_
        }

        # Determine if analysis file
        is_analysis <- "id" %in% names(inference_stats)
        if(is_analysis) {
          message("File already contains IDs skipping propagation.")
          inference_stats$id <- suppressWarnings(as.numeric(as.character(inference_stats$id)))

          original_rows <- inference_stats$prop_type == "original"
          id_counts <- table(inference_stats$id[original_rows])
          keep_ids <- as.integer(names(id_counts[id_counts >= input$min_appearances]))
          inference_stats <- inference_stats[inference_stats$id %in% keep_ids, ]
          df_analysis(inference_stats)
        }

        if(!is_analysis) {
          if(!"frame_num" %in% names(inference_stats)) {
            inference_stats$frame_num <- as.integer(sub(".*_(\\d+)\\.tif$", "\\1", inference_stats$frame))
          }
          inference_stats$points <- apply(inference_stats[, c("x1","y1","x2","y2","x3","y3","x4","y4")],
                                          1, function(r) matrix(r, ncol = 2, byrow = TRUE), simplify = FALSE)
          inference_stats$manual <- inference_stats$manual %||% FALSE
          inference_stats$id <- NA_integer_

          next_id <- 1
          threshold <- input$iou_threshold
          n <- nrow(inference_stats)
          coords_array <- array(NA, dim = c(2, 4, n))
          for (i in seq_len(n)) {
            coords_array[1,,i] <- inference_stats$points[[i]][,1]
            coords_array[2,,i] <- inference_stats$points[[i]][,2]
          }
          inference_stats$x_min <- apply(coords_array[1,,], 2, min)
          inference_stats$x_max <- apply(coords_array[1,,], 2, max)
          inference_stats$y_min <- apply(coords_array[2,,], 2, min)
          inference_stats$y_max <- apply(coords_array[2,,], 2, max)

          shiny::withProgress(message = "Running analysis & propagating boxes...", value = 0, {
            for(i in seq_len(n)) {
              if(is.na(inference_stats$id[i])) {
                inference_stats$id[i] <- next_id
                poly_i <- inference_stats$points[[i]]
                if(i < n) {
                  remaining <- (i+1):n
                  xi_min <- inference_stats$x_min[i]; xi_max <- inference_stats$x_max[i]
                  yi_min <- inference_stats$y_min[i]; yi_max <- inference_stats$y_max[i]
                  x_overlap <- (xi_max >= inference_stats$x_min[remaining]) & (xi_min <= inference_stats$x_max[remaining])
                  y_overlap <- (yi_max >= inference_stats$y_min[remaining]) & (yi_min <= inference_stats$y_max[remaining])
                  candidates <- remaining[x_overlap & y_overlap]
                  for(j in candidates) {
                    if(is.na(inference_stats$id[j]) && obb_iou(poly_i, inference_stats$points[[j]]) > threshold) {
                      inference_stats$id[j] <- inference_stats$id[i]
                    }
                  }
                }
                next_id <- next_id + 1
              }
              shiny::incProgress(1/n)
            }
          })

          id_counts <- table(inference_stats$id)
          keep_ids <- as.integer(names(id_counts[id_counts >= input$min_appearances]))
          inference_stats <- inference_stats[inference_stats$id %in% keep_ids, ]
          df_analysis(inference_stats)
        }
      })

      #### df_prop reactive ####
      df_prop <- shiny::reactive({
        df <- df_analysis()
        shiny::req(!is.null(df), nrow(df) > 0, is.data.frame(df), "id" %in% names(df))
        if("prop_type" %in% names(df)) {
          message("Boxes already propagated skipping propagation")
          return(df)
        }

        total_frames <- seq_len(length(frame_paths()))
        ids <- unique(df$id)
        shiny::req(length(ids) > 0)

        shiny::withProgress(message = "Propagating boxes...", value = 0, {
          interp_list <- lapply(seq_along(ids), function(idx) {
            id <- ids[idx]
            df_id <- df[df$id == id, ]
            orig_frames <- df_id$frame_num
            interp_frames <- seq(min(orig_frames), max(orig_frames))
            interp_df <- data.frame(frame_num = interp_frames, id = id)
            for(col in c("x1","y1","x2","y2","x3","y3","x4","y4")) {
              vals <- df_id[[col]]
              interp_vals <- rep(NA_real_, length(interp_frames))
              if(length(orig_frames) >= 1 && sum(!is.na(vals)) >= 1) {
                interp_vals[match(orig_frames, interp_frames)] <- vals
                for(j in seq_along(interp_vals)) {
                  if(is.na(interp_vals[j]) && j > 1) interp_vals[j] <- interp_vals[j-1]
                }
              }
              interp_df[[col]] <- interp_vals
            }
            interp_df$propagated <- TRUE
            interp_df$prop_type <- "forward"
            interp_df$manual <- any(df_id$manual %||% FALSE)
            for(f in orig_frames) {
              interp_df$prop_type[interp_df$frame_num == f] <- "original"
              interp_df$propagated[interp_df$frame_num == f] <- FALSE
            }
            first_f <- min(orig_frames)
            interp_df$prop_type[interp_df$frame_num == first_f] <- "first"

            if(max(orig_frames) < max(total_frames)) {
              pupa_frames <- seq(max(orig_frames)+1, max(total_frames))
              pupa_df <- data.frame(frame_num = pupa_frames, id = id)
              for(col in c("x1","y1","x2","y2","x3","y3","x4","y4")) {
                last_val <- tail(stats::na.omit(df_id[[col]]), 1)
                pupa_df[[col]] <- rep(last_val, length(pupa_frames))
              }
              pupa_df$propagated <- TRUE
              pupa_df$prop_type <- "pupa"
              pupa_df$manual <- any(df_id$manual %||% FALSE)
              interp_df <- rbind(interp_df, pupa_df)
            }
            shiny::incProgress(1/length(ids))
            interp_df
          })
          do.call(rbind, interp_list)
        })
      })

      #### Update global df_analysis ####
      shiny::observe({ df_analysis(df_prop()) })

      #### Save path ####
      save_path <- shiny::reactive({
        shiny::req(frame_paths())
        base_file <- basename(frame_paths()[1])
        base_name <- tools::file_path_sans_ext(base_file)
        base_name <- gsub("\\..*$", "", base_name)
        candidate <- file.path("flySurvivalApp_output", paste0(base_name, "_processed.txt"))
        counter <- 1
        while(file.exists(candidate)) {
          candidate <- file.path("flySurvivalApp_output", paste0(base_name, "_processed_", counter, ".txt"))
          counter <- counter + 1
        }
        candidate
      })

      #### Save analysis ####
      shiny::observeEvent(input$save_analysis, {
        df <- df_prop()
        shiny::req(!is.null(df))
        list_cols <- vapply(df, is.list, logical(1))
        if(any(list_cols)) df <- df[, !list_cols, drop = FALSE]
        path <- save_path()
        utils::write.table(df, file = path, sep = "\t", row.names = FALSE, quote = FALSE)
        shiny::showNotification(paste("Saved analysis file:", basename(path)), type = "message")
      })

      #### Remove ID ####
      shiny::observeEvent(input$remove_id_btn, {
        shiny::req(!is.null(df_analysis()), !is.na(input$remove_id))
        df <- df_analysis()
        if(input$remove_id %in% df$id) {
          df <- df[df$id != input$remove_id, ]
          df_analysis(df)
          shiny::showNotification(paste("Removed ID:", input$remove_id), type="message")
        } else {
          shiny::showNotification(paste("ID not found:", input$remove_id), type="warning")
        }
      })

      #### Pupariation click ####
      shiny::observeEvent(input$frame_plot_click, {
        shiny::req(df_analysis(), input$frame_idx)
        df <- df_analysis()
        click <- input$frame_plot_click; x_center <- click$x; y_center <- click$y
        new_id <- if(length(df$id)) max(df$id, na.rm=TRUE)+1 else 1
        frame_file <- frame_paths()[[input$frame_idx]]
        frame_num <- input$frame_idx
        img <- tiff::readTIFF(frame_file, as.is=TRUE)
        h <- dim(img)[1]; w <- dim(img)[2]; radius <- min(w,h)*0.05
        x_pts <- c(x_center-radius, x_center+radius, x_center+radius, x_center-radius)
        y_pts <- c(y_center-radius, y_center-radius, y_center+radius, y_center+radius)
        first_row <- data.frame(
          frame=basename(frame_file), frame_num=frame_num, id=new_id,
          x1=x_pts[1], y1=y_pts[1], x2=x_pts[2], y2=y_pts[2],
          x3=x_pts[3], y3=y_pts[3], x4=x_pts[4], y4=y_pts[4],
          propagated=TRUE, prop_type="first", manual=TRUE
        )
        if(length(seq_len(length(frame_paths()))) > 1) {
          pupa_rows <- do.call(rbind, lapply(seq_len(length(frame_paths()))[-1], function(f) {
            data.frame(
              frame=basename(frame_paths()[f]), frame_num=f, id=new_id,
              x1=x_pts[1], y1=y_pts[1], x2=x_pts[2], y2=y_pts[2],
              x3=x_pts[3], y3=y_pts[3], x4=x_pts[4], y4=y_pts[4],
              propagated=TRUE, prop_type="pupa", manual=TRUE
            )
          }))
          new_rows <- rbind(first_row, pupa_rows)
        } else new_rows <- first_row
        missing_cols <- setdiff(names(df), names(new_rows))
        for(col in missing_cols) new_rows[[col]] <- NA
        new_rows <- new_rows[, names(df)]
        df_analysis(rbind(df, new_rows))
        shiny::showNotification(paste("Added pupariation ID", new_id, "from frame", frame_num), type="message")
      })

      #### Render frame ####
      output$frame_plot <- shiny::renderPlot({
        shiny::req(frame_paths(), input$frame_idx, df_prop())
        frame_file <- frame_paths()[[input$frame_idx]]
        frame_num <- input$frame_idx
        img <- tiff::readTIFF(frame_file, as.is=TRUE)
        height <- dim(img)[1]; width <- dim(img)[2]

        graphics::par(xaxs="i", yaxs="i")
        graphics::plot(NA, xlim=c(0,width), ylim=c(height,0), xlab="", ylab="", axes=FALSE, asp=1,
                       main=sprintf("%s (frame %d)", basename(frame_file), frame_num))
        graphics::rasterImage(img, 0, height, width, 0, interpolate=FALSE)

        df <- df_prop()
        boxes <- df[df$frame_num==frame_num, , drop=FALSE]
        if(input$show_only_propagated) boxes <- boxes[boxes$prop_type!="original" & boxes$prop_type!="first", ]

        if(nrow(boxes) > 0){
          for(i in seq_len(nrow(boxes))){
            row <- boxes[i,]
            pts <- matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
            xs <- c(pts[,1], pts[1,1]); ys <- c(pts[,2], pts[1,2])
            col_box <- switch(as.character(row$prop_type), first="purple", original="red", forward="blue", pupa="green")
            graphics::polygon(xs, ys, border=col_box, lwd=2)
            if(isTRUE(row$manual) && row$prop_type=="first") graphics::points(mean(xs), mean(ys), col="purple", pch=16, cex=1.5)
            graphics::text(mean(xs), mean(ys), labels=row$id, col=col_box, cex=0.8)
          }
        }
        graphics::legend("topright",
                         legend=c("First appearance","Currently detected","Interpolated","Interpolated after last image"),
                         col=c("purple","red","blue","green"), lwd=2, cex=0.9, bg="white")
      })
    }
  )
}
