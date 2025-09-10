largest_box_panel <- function(df_storage, current_frame_idx, refresh_browse) {
  list(
    ui = tabPanel(
      "Largest Box Preview",
      fluidPage(
        titlePanel("Show Largest OBB per id"),
        
        # Sidebar layout for folder, file, ID selection and button
        sidebarLayout(
          sidebarPanel(
            uiOutput("frame_folder_ui2"),
            uiOutput("stats_file_ui2"),
            uiOutput("id_selector_ui"),
            actionButton("move_first_btn", "Move First Appearance to Current Frame", class = "btn-warning"),
            width = 4
          ),
          mainPanel(
            h4("Largest Box Crop"),
            plotOutput("largest_box_plot", height = "600px"),
            width = 8
          )
        ),
        
        # Slider row at the bottom
        fluidRow(
          column(
            width = 12,
            uiOutput("frame_slider_ui2")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      .nz <- function(x) if(length(x)) x else ""
      
      #### Frame folder selection ####
      output$frame_folder_ui2 <- renderUI({
        subs <- list.dirs("avi_frames", full.names = FALSE, recursive = FALSE)
        selectInput("frame_folder", "Frame Subfolder:", choices = .nz(subs))
      })
      
      #### Stats file selection (only _analysis files) ####
      output$stats_file_ui2 <- renderUI({
        stats_files <- list.files("statsdir", pattern = "_analysis\\.txt$", full.names = FALSE)
        selectInput("stats_file", "Analysis File:", choices = .nz(stats_files))
      })
      
      #### Frame list reactive ####
      frame_paths <- reactive({
        req(input$frame_folder)
        folder <- file.path("avi_frames", input$frame_folder)
        files <- list.files(folder, pattern="\\.tif$", full.names=TRUE)
        files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", files)))]
      })
      
      #### Frame slider UI (bottom of page) ####
      output$frame_slider_ui2 <- renderUI({
        req(frame_paths())
        div(
          style = "font-size: 16px; margin: 10px 20px;",  # larger font, spacing
          sliderInput(
            "frame_idx", 
            "Frame:", 
            min = 1, 
            max = length(frame_paths()), 
            value = 1, 
            step = 1, 
            animate = TRUE,
            width = "100%"
          )
        )
      })
      
      #### Read analysis file ####
      stats_df <- reactive({
        req(input$stats_file)
        df <- read.table(file.path("statsdir", input$stats_file), header=TRUE, sep="\t")
        
        # Handle raw vs. analysis files
        if ("frame" %in% names(df)) {
          df$frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(df$frame)))
        } else if (!"frame_num" %in% names(df)) {
          stop("File must contain either 'frame' or 'frame_num' column")
        }
        
        df
      })
      
      #### Largest OBB per id ####
      largest_per_id <- reactive({
        req(stats_df())
        df <- stats_df()
        
        id_list <- split(df, df$id)
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
      
      #### id selector ####
      output$id_selector_ui <- renderUI({
        req(largest_per_id())
        selectInput("selected_id", "Choose id:", choices = unique(largest_per_id()$id))
      })
      
      #### Plot largest crop for selected id ####
      output$largest_box_plot <- renderPlot({
        req(largest_per_id(), input$selected_id, input$frame_folder, input$frame_idx)
        
        row <- largest_per_id()[largest_per_id()$id == input$selected_id, ]
        req(nrow(row) == 1)
        
        # Determine the frame file using the slider
        frame_file <- frame_paths()[input$frame_idx]
        req(file.exists(frame_file))
        
        img <- tiff::readTIFF(frame_file, as.is=TRUE)  # RGB array [h, w, 3]
        pts <- matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
        
        # Axis-aligned bounding rectangle around OBB
        x_range <- round(range(pts[,1]))
        y_range <- round(range(pts[,2]))
        
        # Clamp ranges to valid image bounds
        x_range[1] <- max(1, x_range[1])
        x_range[2] <- min(dim(img)[2], x_range[2])
        y_range[1] <- max(1, y_range[1])
        y_range[2] <- min(dim(img)[1], y_range[2])
        
        par(xaxs="i", yaxs="i")
        plot(
          NA, xlim=x_range, ylim=rev(y_range), xlab="", ylab="", axes=FALSE, asp=1,
          main=sprintf("Largest OBB (id %d, Frame %d)", row$id, row$frame_num)
        )
        
        # Crop RGB array
        cropped <- img[y_range[1]:y_range[2], x_range[1]:x_range[2], , drop=FALSE]
        rasterImage(cropped, x_range[1], y_range[2], x_range[2], y_range[1], interpolate=FALSE)
        
        # Draw polygon outline
        polygon(pts[,1], pts[,2], border="red", lwd=2)
      })
      
      #### Move first appearance of selected id ####
      observeEvent(input$move_first_btn, {
        req(df_storage(), input$selected_id, input$frame_idx)
        df <- df_storage()
        
        id <- input$selected_id
        frame_idx <- input$frame_idx
        
        if(!(id %in% df$id)) {
          showNotification(sprintf("id %d not found in current dataframe", id), type="error")
          return()
        }
        
        # Get current frame file + number
        frame_file <- frame_paths()[frame_idx]
        frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_file)))
        
        # Update first appearance for that id
        old_min <- min(df$frame_num[df$id == id])
        row_idx <- which(df$id == id & df$frame_num == old_min)[1]
        df$frame_num[row_idx] <- frame_num
        if ("frame" %in% names(df)) {
          df$frame[row_idx] <- basename(frame_file)
        }
        
        df_storage(df)
        refresh_browse()  # notify browse_panel to re-render
        
        showNotification(sprintf(
          "Moved first appearance of id %d from frame %d to frame %d",
          id, old_min, frame_num
        ), type="message")
      })
    }
  )
}
