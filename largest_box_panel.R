library(shiny)
library(tiff)
library(sf)

browse_panel <- function() {
  list(
    ui = tabPanel(
      "Browse Inferences",
      fluidPage(
        titlePanel("Browse Frames with Bounding Boxes"),
        sidebarLayout(
          sidebarPanel(
            uiOutput("frame_folder_ui"),
            uiOutput("stats_file_ui"),
            numericInput("iou_threshold", "Merging IoU Threshold:", 0.3, min = 0, max = 1, step = 0.01),
            numericInput("min_appearances", "Minimum Appearances per ID:", 2, min = 1, step = 1),
            checkboxInput("show_only_propagated", "Show Only Propagated Boxes", FALSE),
            actionButton("run_analysis", "Run Analysis & Propagate Boxes", class = "btn-success"),
            actionButton("save_analysis", "Save Analysis File", class = "btn-primary"),
            hr(),
            numericInput("remove_id", "ID to Remove:", value = NA, min = 1, step = 1),
            actionButton("remove_id_btn", "Remove ID", class = "btn-warning"),
            uiOutput("frame_slider_ui")
          ),
          mainPanel(
            h4("Frame Preview"),
            plotOutput("frame_plot", height = "600px", click = "frame_plot_click")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      .nz <- function(x) if(length(x)) x else ""
      
      # Frame folder selection
      output$frame_folder_ui <- renderUI({
        subs <- list.dirs("avi_frames", full.names = FALSE, recursive = FALSE)
        subs <- .nz(subs)
        selectInput("frame_folder", "Frame Subfolder:", choices = subs, selected = if(length(subs)) subs[1] else "")
      })
      
      # Stats file selection
      output$stats_file_ui <- renderUI({
        stats_files <- list.files("statsdir", pattern = "\\.txt$", full.names = FALSE)
        stats_files <- .nz(stats_files)
        selectInput("stats_file", "Stats File:", choices = stats_files, selected = if(length(stats_files)) stats_files[1] else "")
      })
      
      # Frame list
      frame_paths <- reactive({
        req(input$frame_folder)
        folder <- file.path("avi_frames", input$frame_folder)
        files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
        files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", files)))]
      })
      
      # Slider
      output$frame_slider_ui <- renderUI({
        req(frame_paths())
        sliderInput("frame_idx", "Frame:", min = 1, max = length(frame_paths()), value = 1, step = 1, animate = TRUE)
      })
      
      # IoU function
      obb_iou <- function(obb1, obb2) {
        safe_poly <- function(obb) {
          tryCatch({ st_polygon(list(rbind(obb, obb[1, ]))) |> st_sfc() |> st_make_valid() }, error = function(e) NULL)
        }
        poly1 <- safe_poly(obb1)
        poly2 <- safe_poly(obb2)
        if(is.null(poly1) || is.null(poly2)) return(0)
        inter <- suppressWarnings(st_intersection(poly1, poly2))
        if(length(inter) == 0) return(0)
        area_inter <- as.numeric(st_area(inter))
        area_union <- as.numeric(st_area(poly1)) + as.numeric(st_area(poly2)) - area_inter
        if(area_union <= 0) return(0)
        area_inter / area_union
      }
      
      # Storage for processed dataframe
      df_storage <- reactiveVal(NULL)
      
      # Run analysis & propagate
      observeEvent(input$run_analysis, {
        req(input$stats_file)
        df <- read.table(file.path("statsdir", input$stats_file), header=TRUE, sep="\t")
        df$frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(df$frame)))
        df$points <- apply(df[, c("x1","y1","x2","y2","x3","y3","x4","y4")], 1, function(r) matrix(r, ncol=2, byrow=TRUE), simplify=FALSE)
        df$manual <- FALSE
        
        threshold <- input$iou_threshold
        df$id <- NA_integer_
        next_id <- 1
        
        withProgress(message = "Running analysis & propagating boxes...", value = 0, {
          n <- nrow(df)
          for(i in seq_len(n)) {
            if(is.na(df$id[i])) {
              df$id[i] <- next_id
              poly_i <- df$points[[i]]
              for(j in (i+1):n) {
                if(is.na(df$id[j]) && obb_iou(poly_i, df$points[[j]]) > threshold) {
                  df$id[j] <- df$id[i]
                }
              }
              next_id <- next_id + 1
            }
            incProgress(1/n, detail = paste("Processing row", i, "of", n))
          }
        })
        
        # Remove low-frequency IDs except manual
        id_counts <- table(df$id)
        keep_ids <- as.integer(names(id_counts[id_counts >= input$min_appearances]))
        manual_ids <- df$id[df$manual]
        keep_ids <- unique(c(keep_ids, manual_ids))
        df <- df[df$id %in% keep_ids, ]
        df_storage(df)
      })
      
      # Remove specific ID
      observeEvent(input$remove_id_btn, {
        req(!is.null(df_storage()), !is.na(input$remove_id))
        df <- df_storage()
        if(input$remove_id %in% df$id) {
          df <- df[df$id != input$remove_id, ]
          df_storage(df)
          showNotification(paste("Removed ID:", input$remove_id), type="message")
        } else {
          showNotification(paste("ID not found:", input$remove_id), type="warning")
        }
      })
      
      # Propagate boxes forward with first-frame, interp, pupa
      df_prop <- reactive({
        df <- df_storage()
        req(!is.null(df), nrow(df) > 0)
        total_frames <- seq_len(length(frame_paths()))
        ids <- unique(df$id)
        
        withProgress(message = "Propagating boxes across frames...", value = 0, {
          interp_list <- lapply(seq_along(ids), function(idx) {
            id <- ids[idx]
            df_id <- df[df$id == id, ]
            orig_frames <- df_id$frame_num
            
            # interpolate between appearances
            interp_frames <- seq(min(orig_frames), max(orig_frames))
            interp_df <- data.frame(frame_num = interp_frames, id = id)
            for(col in c("x1","y1","x2","y2","x3","y3","x4","y4")) {
              interp_df[[col]] <- approx(orig_frames, df_id[[col]], xout = interp_frames)$y
            }
            interp_df$propagated <- TRUE
            interp_df$prop_type <- "interp"
            interp_df$manual <- any(df_id$manual)
            
            # add original frames
            for(f in orig_frames) {
              interp_df$prop_type[interp_df$frame_num == f] <- "original"
              interp_df$propagated[interp_df$frame_num == f] <- FALSE
            }
            # mark first appearance
            first_f <- min(orig_frames)
            interp_df$prop_type[interp_df$frame_num == first_f] <- "first"
            
            # propagate to end as pupa
            if(max(orig_frames) < max(total_frames)) {
              pupa_frames <- seq(max(orig_frames)+1, max(total_frames))
              pupa_df <- data.frame(frame_num = pupa_frames, id = id)
              for(col in c("x1","y1","x2","y2","x3","y3","x4","y4")) {
                pupa_df[[col]] <- approx(orig_frames, df_id[[col]], xout = pupa_frames, rule=2)$y
              }
              pupa_df$propagated <- TRUE
              pupa_df$prop_type <- "pupa"
              pupa_df$manual <- any(df_id$manual)
              interp_df <- rbind(interp_df, pupa_df)
            }
            
            incProgress(1/length(ids), detail = paste("Propagating ID", id, "of", length(ids)))
            interp_df
          })
          
          do.call(rbind, interp_list)
        })
      })
      
      # Save analysis file
      observeEvent(input$save_analysis, {
        df <- df_prop()
        req(!is.null(df))
        base_name <- tools::file_path_sans_ext(basename(input$stats_file))
        if(grepl("_analysis$", base_name)) {
          save_name <- paste0(base_name, ".txt")
        } else {
          save_name <- paste0(base_name, "_analysis.txt")
        }
        write.table(df, file=file.path("statsdir", save_name), sep="\t", row.names=FALSE, quote=FALSE)
        showNotification(paste("Saved analysis file:", save_name), type="message")
      })
      
      # Add pupariation on click and propagate
      observeEvent(input$frame_plot_click, {
        req(df_storage(), input$frame_idx)
        df <- df_storage()
        click <- input$frame_plot_click
        x_center <- click$x
        y_center <- click$y
        
        # New ID
        new_id <- if(length(df$id)) max(df$id, na.rm=TRUE) + 1 else 1
        
        # Current frame number
        frame_file <- frame_paths()[[input$frame_idx]]
        frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_file)))
        total_frames <- seq(frame_num, length(frame_paths()))
        
        # Circle radius
        img <- tiff::readTIFF(frame_file, as.is=TRUE)
        h <- dim(img)[1]; w <- dim(img)[2]
        radius <- min(w,h) * 0.05
        
        # Simple 4-point polygon
        x_pts <- c(x_center-radius, x_center+radius, x_center+radius, x_center-radius)
        y_pts <- c(y_center-radius, y_center-radius, y_center+radius, y_center+radius)
        
        # First frame (purple)
        first_row <- data.frame(
          frame = basename(frame_file),
          frame_num = frame_num,
          id = new_id,
          x1 = x_pts[1], y1 = y_pts[1],
          x2 = x_pts[2], y2 = y_pts[2],
          x3 = x_pts[3], y3 = y_pts[3],
          x4 = x_pts[4], y4 = y_pts[4],
          propagated = TRUE,
          prop_type = "first",
          manual = TRUE
        )
        
        # Pupa frames (green)
        if(length(total_frames) > 1) {
          pupa_rows <- do.call(rbind, lapply(total_frames[-1], function(f) {
            data.frame(
              frame = basename(frame_paths()[f]),
              frame_num = f,
              id = new_id,
              x1 = x_pts[1], y1 = y_pts[1],
              x2 = x_pts[2], y2 = y_pts[2],
              x3 = x_pts[3], y3 = y_pts[3],
              x4 = x_pts[4], y4 = y_pts[4],
              propagated = TRUE,
              prop_type = "pupa",
              manual = TRUE
            )
          }))
          new_rows <- rbind(first_row, pupa_rows)
        } else {
          new_rows <- first_row
        }
        
        # Ensure column consistency
        missing_cols <- setdiff(names(df), names(new_rows))
        for(col in missing_cols) new_rows[[col]] <- NA
        new_rows <- new_rows[, names(df)]
        
        df_storage(rbind(df, new_rows))
        showNotification(paste("Added pupariation ID", new_id, "from frame", frame_num, "to end"), type="message")
      })
      
      # Render frame
      output$frame_plot <- renderPlot({
        req(frame_paths(), input$frame_idx, df_prop())
        frame_file <- frame_paths()[[input$frame_idx]]
        frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_file)))
        
        img <- tiff::readTIFF(frame_file, as.is=TRUE)
        dims <- dim(img)
        height <- dims[1]; width <- dims[2]
        
        par(xaxs="i", yaxs="i")
        plot(NA, xlim=c(0,width), ylim=c(height,0), xlab="", ylab="", axes=FALSE, asp=1,
             main=sprintf("%s (frame %d)", basename(frame_file), frame_num))
        rasterImage(img, 0, height, width, 0, interpolate=FALSE)
        
        df <- df_prop()
        boxes <- df[df$frame_num==frame_num, , drop=FALSE]
        if(input$show_only_propagated) boxes <- boxes[boxes$prop_type != "original" & boxes$prop_type != "first", ]
        
        if(nrow(boxes) > 0) {
          for(i in seq_len(nrow(boxes))) {
            row <- boxes[i, ]
            pts <- matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
            xs <- c(pts[,1], pts[1,1])
            ys <- c(pts[,2], pts[1,2])
            
            col_box <- switch(as.character(row$prop_type),
                              first    = "purple",
                              original = "red",
                              interp   = "blue",
                              pupa     = "green")
            
            polygon(xs, ys, border=col_box, lwd=2)
            
            # Draw purple circle for manual first frame
            if(row$manual && row$prop_type=="first") {
              points(mean(xs), mean(ys), col="purple", pch=16, cex=1.5)
            }
            
            text(mean(xs), mean(ys), labels=row$id, col=col_box, cex=0.8)
          }
        }
        
        legend("topright", legend=c("First appearance","Currently detected","Interpolated","Interpolated after last image"),
               col=c("purple","red","blue","green"), lwd=2, cex=0.9, bg="white")
      })
    }
  )
}
