largest_box_panel <- function(df_storage, current_frame_idx, refresh_browse) {
  list(
    ui = tabPanel(
      "Largest Box Preview",
      fluidPage(
        titlePanel("Show Largest OBB per id"),
        
        sidebarLayout(
          sidebarPanel(
            uiOutput("frame_folder_ui2"),
            uiOutput("stats_file_ui2"),
            actionButton("reload_stats_btn", "Load/Reload Dataset", class = "btn-primary"),
            uiOutput("id_selector_ui"),
            actionButton(
              "move_first_btn",
              "Move First Appearance to Current Frame",
              class = "btn-warning"
            ),
            width = 4
          ),
          mainPanel(
            h4("Largest Box Crop"),
            plotOutput("largest_box_plot", height = "600px"),
            width = 8
          )
        ),
        
        fluidRow(
          column(width = 12, uiOutput("frame_slider_ui2"))
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
      
      #### Stats file selection ####
      output$stats_file_ui2 <- renderUI({
        stats_files <- list.files("statsdir", pattern="_analysis*\\.txt$", full.names=FALSE)
        selectInput("stats_file", "Analysis File:", choices=.nz(stats_files))
      })
      
      #### Frame paths ####
      frame_paths <- reactive({
        req(input$frame_folder)
        folder <- file.path("avi_frames", input$frame_folder)
        files <- list.files(folder, pattern="\\.tif$", full.names=TRUE)
        files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", files)))]
      })
      
      #### Frame slider UI ####
      output$frame_slider_ui2 <- renderUI({
        req(frame_paths())
        sliderInput("frame_idx", "Frame:",
                    min=1, max=length(frame_paths()), value=1, step=1, animate=TRUE, width="100%")
      })
      
      #### Stats dataframe reactive ####
      stats_df <- reactiveVal(NULL)  # initialize reactiveVal
      
      # Function to load stats
      load_stats <- function(file) {
        req(file)
        df <- read.table(file.path("statsdir", file),
                         header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        
        df$frame_num <- as.numeric(df$frame_num)
        showNotification("Dataset loaded", type = "message")
        df
      }

      # Reload dataset when button clicked
      observeEvent(input$reload_stats_btn, {
        stats_df(load_stats(input$stats_file))
      })
      
      #### Reset frame slider when dataset or frames change ####
      observeEvent(list(stats_df(), frame_paths()), {
        req(frame_paths())
        # Default: first frame
        updateSliderInput(session, "frame_idx", min=1, max=length(frame_paths()), value=1)
      })
      
      #### Largest OBB per ID ####
      largest_per_id <- reactive({
        df <- stats_df()
        req(df)
        id_list <- split(df, df$id)
        out_list <- lapply(id_list, function(df_id) {
          if(nrow(df_id)==0) return(NULL)
          areas <- apply(df_id[, c("x1","y1","x2","y2","x3","y3","x4","y4")], 1, function(r){
            pts <- matrix(as.numeric(r), ncol=2, byrow=TRUE)
            poly <- tryCatch(sf::st_polygon(list(rbind(pts, pts[1,]))), error=function(e) NULL)
            if(is.null(poly)) return(0)
            as.numeric(sf::st_area(poly))
          })
          df_id[which.max(areas), , drop=FALSE]
        })
        out_list <- Filter(Negate(is.null), out_list)
        if(length(out_list)==0) return(NULL)
        do.call(rbind, out_list)
      })
      
      #### ID selector ####
      output$id_selector_ui <- renderUI({
        req(largest_per_id())
        selectInput("selected_id", "Choose id:", choices=unique(largest_per_id()$id))
      })
      
      #### Automatically select first ID and move slider to its first frame after loading ####
      observeEvent(stats_df(), {
        df <- stats_df()
        req(df)
        ids <- unique(df$id)
        if(length(ids) > 0) {
          updateSelectInput(session, "selected_id", selected = ids[1])
          
          first_frame <- df$frame_num[df$id==ids[1] & df$prop_type=="first"]
          if(length(first_frame)) {
            idx <- which.min(abs(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_paths()))) - first_frame))
            if(length(idx)==1) updateSliderInput(session, "frame_idx", value=idx)
          }
        }
      })
      
      #### Auto-move slider to first appearance when ID changes ####
      observeEvent(input$selected_id, {
        df <- stats_df()
        req(df)
        first_frame <- df$frame_num[df$id==input$selected_id & df$prop_type=="first"]
        if(length(first_frame)) {
          idx <- which.min(abs(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_paths()))) - first_frame))
          if(length(idx)==1) updateSliderInput(session, "frame_idx", value=idx)
        }
      })
      
      #### Plot largest box for selected ID ####
      output$largest_box_plot <- renderPlot({
        req(largest_per_id(), input$selected_id, input$frame_idx)
        row <- largest_per_id()[largest_per_id()$id==input$selected_id,]
        req(nrow(row)==1)
        
        frame_file <- frame_paths()[input$frame_idx]
        req(file.exists(frame_file))
        
        img <- tiff::readTIFF(frame_file, as.is=TRUE)
        pts <- matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
        
        x_range <- round(range(pts[,1])); y_range <- round(range(pts[,2]))
        x_range <- pmax(1, pmin(dim(img)[2], x_range))
        y_range <- pmax(1, pmin(dim(img)[1], y_range))
        
        par(xaxs="i", yaxs="i")
        plot(NA, xlim=x_range, ylim=rev(y_range), xlab="", ylab="", axes=FALSE, asp=1,
             main=sprintf("Largest OBB (id %s, Frame %s)", row$id, input$frame_idx))
        cropped <- img[y_range[1]:y_range[2], x_range[1]:x_range[2], , drop=FALSE]
        rasterImage(cropped, x_range[1], y_range[2], x_range[2], y_range[1], interpolate=FALSE)
        
        df <- stats_df()
        req(df)
        first_frame <- df$frame_num[df$id==input$selected_id & df$prop_type=="first"]
        border_color <- if(length(first_frame) && df$frame_num[df$id==input$selected_id & df$prop_type=="first"] == as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_file)))) "purple" else "red"
        
        polygon(pts[,1], pts[,2], border=border_color, lwd=2)
      })
      
      #### Move first appearance ####
      observeEvent(input$move_first_btn, {
        req(df_storage, input$selected_id, input$frame_idx, input$stats_file)
        df <- df_storage()
        id <- input$selected_id
        frame_file <- frame_paths()[input$frame_idx]
        frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(frame_file)))
        
        # Clear previous first
        df$prop_type[df$id==id & df$prop_type=="first"] <- "original"
        
        # Set new first appearance at current frame
        idx <- which(df$id==id & df$frame_num==frame_num)
        if(length(idx)==0) idx <- which(df$id==id)[1]  # fallback
        df$prop_type[idx] <- "first"
        
        df_storage(df)
        refresh_browse()
        
        # Save dataset safely
        tryCatch({
          write.table(df, file=file.path("statsdir", input$stats_file),
                      sep="\t", row.names=FALSE, quote=FALSE)
        }, error=function(e){
          showNotification(paste("Error saving file:", e$message), type="error")
        })
        
        showNotification(sprintf("Moved first appearance of id %s to frame %d", id, frame_num), type="message")
      })
      
    }
  )
}
