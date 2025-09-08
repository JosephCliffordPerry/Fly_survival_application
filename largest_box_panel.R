largest_box_panel <- function(df_storage, current_frame_idx, refresh_browse) {
  list(
    ui = tabPanel(
      "Largest Box Preview",
      fluidPage(
        titlePanel("Show Largest OBB per shape_id"),
        sidebarLayout(
          sidebarPanel(
            uiOutput("frame_folder_ui"),
            uiOutput("stats_file_ui"),
            uiOutput("id_selector_ui"),
            actionButton("move_first_btn", "Move First Appearance to Current Frame", class = "btn-warning")
          ),
          mainPanel(
            h4("Largest Box Crop"),
            plotOutput("largest_box_plot", height = "600px")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      .nz <- function(x) if(length(x)) x else ""
      
      # Frame folder selection
      output$frame_folder_ui <- renderUI({
        subs <- list.dirs("avi_frames", full.names = FALSE, recursive = FALSE)
        selectInput("frame_folder", "Frame Subfolder:", choices = .nz(subs))
      })
      
      # Stats file selection
      output$stats_file_ui <- renderUI({
        stats_files <- list.files("statsdir", pattern = "\\.txt$", full.names = FALSE)
        selectInput("stats_file", "Stats File:", choices = .nz(stats_files))
      })
      
      # Read stats file
      stats_df <- reactive({
        req(input$stats_file)
        df <- read.table(file.path("statsdir", input$stats_file), header=TRUE, sep="\t")
        df$frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(df$frame)))
        df
      })
      
      # Largest OBB per shape_id across all frames
      largest_per_id <- reactive({
        req(stats_df())
        df <- stats_df()
        
        id_list <- split(df, df$shape_id)
        out_list <- lapply(id_list, function(df_id) {
          if(nrow(df_id) == 0) return(NULL)
          areas <- apply(df_id[, c("x1","y1","x2","y2","x3","y3","x4","y4")], 1, function(r) {
            pts <- matrix(r, ncol=2, byrow=TRUE)
            poly <- tryCatch(sf::st_polygon(list(rbind(pts, pts[1,]))), error = function(e) NULL)
            if (is.null(poly)) return(0)
            as.numeric(sf::st_area(poly))
          })
          df_id[which.max(areas), , drop = FALSE]
        })
        
        out_list <- Filter(Negate(is.null), out_list)
        if (length(out_list) == 0) return(NULL)
        
        do.call(rbind, out_list)
      })
      
      # shape_id selector
      output$id_selector_ui <- renderUI({
        req(largest_per_id())
        selectInput("selected_id", "Choose shape_id:", choices = unique(largest_per_id()$shape_id))
      })
      
      # Plot largest crop for selected shape_id
      output$largest_box_plot <- renderPlot({
        req(largest_per_id(), input$selected_id, input$frame_folder)
        
        row <- largest_per_id()[largest_per_id()$shape_id == input$selected_id, ]
        req(nrow(row) == 1)
        
        frame_file <- file.path("avi_frames", input$frame_folder, basename(row$frame))
        req(file.exists(frame_file))
        
        img <- tiff::readTIFF(frame_file, as.is=TRUE)
        pts <- matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
        
        # Axis-aligned bounding rectangle around OBB
        x_range <- range(pts[,1])
        y_range <- range(pts[,2])
        
        par(xaxs="i", yaxs="i")
        plot(NA, xlim=x_range, ylim=rev(y_range), xlab="", ylab="", axes=FALSE, asp=1,
             main=sprintf("Largest OBB (shape_id %d, Frame %d)", row$shape_id, row$frame_num))
        
        cropped <- img[y_range[1]:y_range[2], x_range[1]:x_range[2], drop=FALSE]
        rasterImage(cropped, x_range[1], y_range[2], x_range[2], y_range[1], interpolate=FALSE)
        
        polygon(pts[,1], pts[,2], border="red", lwd=2)
      })
      
      # Move first appearance of selected shape_id to current frame
      observeEvent(input$move_first_btn, {
        req(df_storage(), input$selected_id, current_frame_idx())
        df <- df_storage()
        
        id <- input$selected_id
        frame_idx <- current_frame_idx()
        
        if(!(id %in% df$shape_id)) {
          showNotification(sprintf("shape_id %d not found in current dataframe", id), type="error")
          return()
        }
        
        # Get current frame file + number
        frame_files <- list.files(file.path("avi_frames", input$frame_folder), pattern="\\.tif$", full.names=TRUE)
        frame_files <- frame_files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", frame_files)))]
        frame_file <- frame_files[[frame_idx]]
        frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_file)))
        
        # Update first appearance for that shape_id
        old_min <- min(df$frame_num[df$shape_id == id])
        row_idx <- which(df$shape_id == id & df$frame_num == old_min)[1]
        df$frame_num[row_idx] <- frame_num
        df$frame[row_idx] <- basename(frame_file)
        
        df_storage(df)
        refresh_browse()  # notify browse_panel to re-render
        
        showNotification(sprintf(
          "Moved first appearance of shape_id %d from frame %d to frame %d",
          id, old_min, frame_num
        ), type="message")
      })
    }
  )
}
