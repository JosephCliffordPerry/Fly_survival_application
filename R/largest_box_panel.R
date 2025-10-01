largest_box_panel <- function(df_analysis, frame_paths, stats_file=NULL) {
  list(
    ui = shiny::tabPanel(
      "Individual Pupa Browser",
      shiny::fluidPage(
        shiny::titlePanel("Show Largest OBB per ID"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("id_selector_ui"),
            shiny::actionButton(
              "move_first_btn",
              "Move First Appearance to Current Frame",
              class = "btn-warning"
            ),
            width = 4
          ),
          shiny::mainPanel(
            shiny::h4("Largest Box Crop"),
            shiny::plotOutput("largest_box_plot", height = "600px"),
            width = 8
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 12, shiny::uiOutput("frame_slider_ui2"))
        )
      )
    ),

    server = function(input, output, session) {
      .nz <- function(x) if (length(x)) x else ""

      #### Frame slider ####
      output$frame_slider_ui2 <- shiny::renderUI({
        shiny::sliderInput(
          "frame_box_idx", "Frame:",
          min = 1, max = length(frame_paths()), value = 1,
          step = 1, animate = TRUE, width = "100%"
        )
      })

      #### Largest OBB per ID ####
      largest_per_id <- shiny::reactive({
        df <- df_analysis()
        shiny::req(df$id)
        id_list <- base::split(df, df$id)
        out_list <- base::lapply(id_list, function(df_id) {
          if (nrow(df_id) == 0) return(NULL)
          areas <- base::apply(df_id[, c("x1","y1","x2","y2","x3","y3","x4","y4")], 1, function(r) {
            pts <- base::matrix(as.numeric(r), ncol = 2, byrow = TRUE)
            poly <- tryCatch(sf::st_polygon(list(base::rbind(pts, pts[1,]))), error=function(e) NULL)
            if (is.null(poly)) return(0)
            as.numeric(sf::st_area(poly))
          })
          df_id[base::which.max(areas), , drop=FALSE]
        })
        out_list <- base::Filter(base::Negate(base::is.null), out_list)
        if (length(out_list) == 0) return(NULL)
        base::do.call(base::rbind, out_list)
      })

      #### ID selector ####
      output$id_selector_ui <- shiny::renderUI({
        lp <- largest_per_id()
        shiny::req(lp)
        shiny::selectInput("selected_id", "Choose id:", choices = base::unique(lp$id))
      })

      #### Slider auto-update ####
      shiny::observe({
        df <- df_analysis()
        shiny::req(df$id)
        lp <- largest_per_id()
        shiny::req(df, lp)
        ids <- base::unique(lp$id)
        if (length(ids) == 0) return()

        selected_id <- if (!is.null(input$selected_id) && input$selected_id %in% ids) {
          input$selected_id
        } else ids[1]

        if (is.null(input$selected_id) || input$selected_id != selected_id) {
          shiny::updateSelectInput(session, "selected_id", selected = selected_id)
        }

        first_frame <- df$frame_num[df$id == selected_id & df$prop_type == "first"]
        if (length(first_frame) == 1 && !is.null(first_frame)) {
          shiny::updateSliderInput(session, "frame_box_idx", value = first_frame)
        }
      })

      #### Plot largest box ####
      output$largest_box_plot <- shiny::renderPlot({
        lp <- largest_per_id()
        shiny::req(lp, input$selected_id, input$frame_box_idx)

        row <- lp[lp$id == input$selected_id, ]
        shiny::req(base::nrow(row) == 1)

        frame_file <- frame_paths()[input$frame_box_idx]
        shiny::req(base::file.exists(frame_file))
        img <- tiff::readTIFF(frame_file, as.is = TRUE)

        pts <- base::matrix(as.numeric(row[c("x1","y1","x2","y2","x3","y3","x4","y4")]), ncol=2, byrow=TRUE)
        x_range <- base::pmax(1, base::pmin(dim(img)[2], base::round(base::range(pts[,1]))))
        y_range <- base::pmax(1, base::pmin(dim(img)[1], base::round(base::range(pts[,2]))))

        graphics::par(xaxs="i", yaxs="i")
        graphics::plot(NA, xlim=x_range, ylim=base::rev(y_range), xlab="", ylab="", axes=FALSE, asp=1,
                       main=base::sprintf("Largest OBB (id %s, Frame %s)", row$id, input$frame_box_idx))
        cropped <- img[y_range[1]:y_range[2], x_range[1]:x_range[2], , drop=FALSE]
        graphics::rasterImage(cropped, x_range[1], y_range[2], x_range[2], y_range[1], interpolate=FALSE)

        df <- df_analysis()
        first_frame <- df$frame_num[df$id == input$selected_id & df$prop_type == "first"]
        border_color <- if (length(first_frame) && first_frame == input$frame_box_idx) "purple" else "red"
        graphics::polygon(pts[,1], pts[,2], border=border_color, lwd=2)
      })

      #### Move first appearance ####
      shiny::observeEvent(input$move_first_btn, {
        shiny::req(df_analysis(), input$selected_id, input$frame_box_idx)
        df <- df_analysis()
        id <- input$selected_id
        frame_num <- input$frame_box_idx

        df$prop_type[df$id == id & df$prop_type == "first"] <- "original"
        frame_to_change <- base::which(df$id == id & df$frame_num == frame_num)
        df$prop_type[frame_to_change] <- "first"

        df_analysis(base::as.data.frame(df))
        shiny::showNotification(base::sprintf("Moved first appearance of id %s to frame %d", id, frame_num), type="message")
      })
    }
  )
}
