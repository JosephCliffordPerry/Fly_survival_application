largest_box_panel <- function(df_analysis, frame_paths, stats_file=NULL) {
  list(
    ui = tabPanel(
      "Individual Pupa Browser",
      fluidPage(
        titlePanel("Show Largest OBB per ID"),
        sidebarLayout(
          sidebarPanel(
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
      .nz <- function(x) if (length(x)) x else ""
      
      #### Frame slider ####
      output$frame_slider_ui2 <- renderUI({
        sliderInput(
          "frame_box_idx", "Frame:",
          min = 1, max = length(frame_paths()), value = 1,
          step = 1, animate = TRUE, width = "100%"
        )
      })
      
      #### Largest OBB per ID ####
      largest_per_id <- reactive({
        df <- df_analysis()
        req(df$id)
        id_list <- split(df, df$id)
        out_list <- lapply(id_list, function(df_id) {
          if (nrow(df_id) == 0) return(NULL)
          areas <- apply(df_id[, c("x1","y1","x2","y2","x3","y3","x4","y4")], 1, function(r) {
            pts <- matrix(as.numeric(r), ncol = 2, byrow = TRUE)
            poly <- tryCatch(sf::st_polygon(list(rbind(pts, pts[1,]))), error=function(e) NULL)
            if (is.null(poly)) return(0)
            as.numeric(sf::st_area(poly))
          })
          df_id[which.max(areas), , drop=FALSE]
        })
        out_list <- Filter(Negate(is.null), out_list)
        if (length(out_list) == 0) return(NULL)
        do.call(rbind, out_list)
      })
      
      #### ID selector ####
      output$id_selector_ui <- renderUI({
        lp <- largest_per_id()
        req(lp)
        selectInput("selected_id", "Choose id:", choices = unique(lp$id))
      })
      
      #### Slider auto-update on load and ID change ####
      observe({
        df <- df_analysis()
        req(df$id)
        lp <- largest_per_id()
        req(df, lp)
        ids <- unique(lp$id)
        if (length(ids) == 0) return()
        
        # Determine selected ID
        selected_id <- if (!is.null(input$selected_id) && input$selected_id %in% ids) {
          input$selected_id
        } else ids[1]
        
        # Update selected_id in UI if necessary
        if (is.null(input$selected_id) || input$selected_id != selected_id) {
          updateSelectInput(session, "selected_id", selected = selected_id)
        }
        
        # Move slider to the frame where prop_type == "first"
        first_frame <- df$frame_num[df$id == selected_id & df$prop_type == "first"]
        if (length(first_frame) == 1 && !is.null(first_frame)) {
          updateSliderInput(session, "frame_box_idx", value = first_frame)
        }
      })
      
      #### Plot largest box ####
      output$largest_box_plot <- renderPlot({
        lp <- largest_per_id()
        req(lp, input$selected_id, input$frame_box_idx)
        
        row <- lp[lp$id == input$selected_id, ]
        req(nrow(row) == 1)
        
        frame_file <- frame_paths()[input$frame_box_idx]
        req(file.exists(frame_file))
        img <- tiff::readTIFF(frame_file, as.is = TRUE)
        
        pts <- matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
        x_range <- pmax(1, pmin(dim(img)[2], round(range(pts[,1]))))
        y_range <- pmax(1, pmin(dim(img)[1], round(range(pts[,2]))))
        
        par(xaxs="i", yaxs="i")
        plot(NA, xlim=x_range, ylim=rev(y_range), xlab="", ylab="", axes=FALSE, asp=1,
             main=sprintf("Largest OBB (id %s, Frame %s)", row$id, input$frame_box_idx))
        cropped <- img[y_range[1]:y_range[2], x_range[1]:x_range[2], , drop=FALSE]
        rasterImage(cropped, x_range[1], y_range[2], x_range[2], y_range[1], interpolate=FALSE)
        
        # Draw the OBB with purple border if first appearance
        df <- df_analysis()
        first_frame <- df$frame_num[df$id == input$selected_id & df$prop_type == "first"]
        border_color <- if (length(first_frame) && first_frame == input$frame_box_idx) "purple" else "red"
        polygon(pts[,1], pts[,2], border=border_color, lwd=2)
      })
      
      #### Move first appearance ####
      observeEvent(input$move_first_btn, {
        req(df_analysis(), input$selected_id, input$frame_box_idx)
        df <- df_analysis()
        id <- input$selected_id
        frame_num <- input$frame_box_idx
        
        # Reset old first appearance
        df$prop_type[df$id == id & df$prop_type == "first"] <- "original"
        
        # Set current frame as first
        frame_to_change <- which(df$id == id & df$frame_num == frame_num)
        df$prop_type[frame_to_change] <- "first"
        
        df_analysis(as.data.frame(df))
        showNotification(sprintf("Moved first appearance of id %s to frame %d", id, frame_num), type="message")
      })
    }
  )
}
