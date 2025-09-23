library(shiny)
library(tiff)
library(openxlsx)
library(lubridate)
library(ggplot2)

graph_panel <- function(df_analysis, frame_paths) {
  list(
    ui = tabPanel(
      "Export",
      fluidPage(
        titlePanel("Dataset formatting"),
        sidebarLayout(
          sidebarPanel(
            dateInput("start_date", "Start Date:", value = Sys.Date()),
            textInput("start_time", "Start Time (HH:MM):", value = "08:00"),
            sliderInput("chamber_split", "Chamber Split:", min = 0, max = 1000, value = 500, step = 1, width = "100%"),
            
            h4("Left Side Info"),
            textInput("left_treatment", "Treatment", ""),
            numericInput("left_dose", "Dose", 0),
            textInput("left_dose_unit", "Dose Unit", "mM"),
            textInput("left_genotype", "Genotype", "wild-type"),
            numericInput("left_replicate", "Replicate", 1),
            
            h4("Right Side Info"),
            textInput("right_treatment", "Treatment", ""),
            numericInput("right_dose", "Dose", 0),
            textInput("right_dose_unit", "Dose Unit", "mM"),
            textInput("right_genotype", "Genotype", "wild-type"),
            numericInput("right_replicate", "Replicate", 1),
  
            actionButton("export_excel", "Export Excel", class = "btn-primary"),
            width = 4
          ),
          mainPanel(
            h4("Frame Preview"),
            plotOutput("frame_plot2", height = "500px", width = "100%"),
            h4("Cumulative First Pupariations"),
            plotOutput("frame_cumsum", height = "400px", width = "100%")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      
      img_width <- reactiveVal(1000)
      first_frame <- reactive({
        req(frame_paths())
        tiff::readTIFF(frame_paths()[1], as.is = TRUE)
      })
      
      observe({
        req(first_frame())
        img <- first_frame()
        img_width(dim(img)[2])
        updateSliderInput(session, "chamber_split", max = img_width(), value = round(img_width()/2))
      })
      
      # ---- Render Frame ----
      output$frame_plot2 <- renderPlot({
        req(df_analysis(), first_frame())
        img <- first_frame()
        h <- dim(img)[1]; w <- dim(img)[2]
        
        par(xaxs = "i", yaxs = "i")
        plot(NA, xlim = c(0, w), ylim = c(h, 0), xlab = "", ylab = "", axes = FALSE, asp = 1)
        rasterImage(img, 0, h, w, 0, interpolate = FALSE)
        abline(v = input$chamber_split, col = "blue", lwd = 2, lty = 2)
      }, res = 96)
      
      # ---- Cumulative sum ----
      output$frame_cumsum <- renderPlot({
        req(df_analysis())
        # Check if required columns exist
        if(!all(c("frame_num", "id") %in% names(df))) {
          showNotification("Analysis hasn't been run yet, so export panel is not ready", type = "warning")
        } else {
        df <- df_analysis()
        first <- aggregate(frame_num ~ id, data = df, FUN = min)
        df_first <- merge(first, df, by = c("id", "frame_num"))
        
        vline <- input$chamber_split
        df_first$side <- ifelse((df_first$x1 + df_first$x2 + df_first$x3 + df_first$x4)/4 <= vline, "Left", "Right")
        
        start_dt <- lubridate::ymd_hm(paste(input$start_date, input$start_time))
        df_first$datetime <- start_dt + lubridate::minutes((df_first$frame_num - min(df$frame_num)) * 5)
        
        df_cum <- df_first |>
          dplyr::arrange(datetime) |>
          dplyr::group_by(side) |>
          dplyr::mutate(cumN = dplyr::row_number())
        
        ggplot(df_cum, aes(x = datetime, y = cumN, colour = side)) +
          geom_step(linewidth = 1.2) +
          labs(x = "Time", y = "Cumulative pupariations", colour = "Side") +
          theme_minimal(base_size = 14)
      }})
      
      
      # Reactive to compute save path based on frame_paths() which ensures reasonable save names using reactivity chains 
      Excel_save_path <- reactive({
        req(frame_paths())
        
        # Derive a base name from the first frame_path
        base_file <- basename(frame_paths()[1])
        base_name <- tools::file_path_sans_ext(base_file)
        base_name <- gsub("\\..*$", "", base_name)  # extra cleanup
        
        # Start with "_export.xlxs"
        candidate <- file.path("statsdir", paste0(base_name, "_export.xlsx"))
        
        # If exists, increment until unique
        counter <- 1
        while (file.exists(candidate)) {
          candidate <- file.path("statsdir", paste0(base_name, "_export_", counter, ".xlsx"))
          counter <- counter + 1
        }
        
        candidate
      })
      # ---- Export Excel ----
      observeEvent(input$export_excel, {
        req(df_analysis())
        df <- df_analysis()
        first_appearance <- aggregate(frame_num ~ id, data = df, FUN = min)
        df_first <- merge(first_appearance, df, by = c("id", "frame_num"))
        
        vline_x <- input$chamber_split
        df_first$side <- ifelse((df_first$x1 + df_first$x2 + df_first$x3 + df_first$x4)/4 <= vline_x, "Left", "Right")
        df_first$Treatment <- ifelse(df_first$side == "Left", input$left_treatment, input$right_treatment)
        df_first$Dose <- ifelse(df_first$side == "Left", input$left_dose, input$right_dose)
        df_first$Dose_unit <- ifelse(df_first$side == "Left", input$left_dose_unit, input$right_dose_unit)
        df_first$Genotype <- ifelse(df_first$side == "Left", input$left_genotype, input$right_genotype)
        df_first$Replicate <- ifelse(df_first$side == "Left", input$left_replicate, input$right_replicate)
        
        start_datetime <- lubridate::ymd_hm(paste(input$start_date, input$start_time))
        df_first$datetime <- start_datetime + lubridate::minutes((df_first$frame_num - min(df$frame_num)) * 5)
        df_first$Date <- format(df_first$datetime, "%d.%m.%Y")
        df_first$Time <- format(df_first$datetime, "%H:%M")
        df_first$Event <- 1
        
        left_table <- df_first[df_first$side == "Left", c("Date","Time","Event","Treatment","Dose","Dose_unit","Genotype","Replicate")]
        right_table <- df_first[df_first$side == "Right", c("Date","Time","Event","Treatment","Dose","Dose_unit","Genotype","Replicate")]
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Left")
        openxlsx::addWorksheet(wb, "Right")
        openxlsx::writeData(wb, "Left", left_table)
        openxlsx::writeData(wb, "Right", right_table)
        openxlsx::saveWorkbook(wb, Excel_save_path(), overwrite = TRUE)
        
        showNotification("Excel exported.", type = "message")
      })
    }
  )
}
