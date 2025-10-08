graph_panel <- function(df_analysis, frame_paths) {
  list(
    ui = shiny::tabPanel(
      "Export",
      shiny::fluidPage(
        shiny::titlePanel("Dataset formatting"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateInput("start_date", "Start Date:", value = base::Sys.Date()),
            shiny::textInput("start_time", "Start Time (HH:MM):", value = "08:00"),
            shiny::sliderInput("chamber_split", "Chamber Split:", min = 0, max = 1000, value = 500, step = 1, width = "100%"),

            shiny::h4("Left Side Info"),
            shiny::textInput("left_treatment", "Treatment", ""),
            shiny::numericInput("left_dose", "Dose", 0),
            shiny::textInput("left_dose_unit", "Dose Unit", "mM"),
            shiny::textInput("left_genotype", "Genotype", "wild-type"),
            shiny::numericInput("left_replicate", "Replicate", 1),

            shiny::h4("Right Side Info"),
            shiny::textInput("right_treatment", "Treatment", ""),
            shiny::numericInput("right_dose", "Dose", 0),
            shiny::textInput("right_dose_unit", "Dose Unit", "mM"),
            shiny::textInput("right_genotype", "Genotype", "wild-type"),
            shiny::numericInput("right_replicate", "Replicate", 1),

            shiny::actionButton("export_excel", "Export Excel", class = "btn-primary"),
            width = 4
          ),
          shiny::mainPanel(
            shiny::h4("Frame Preview"),
            shiny::plotOutput("frame_plot2", height = "500px", width = "100%"),
            shiny::h4("Cumulative First Pupariations"),
            shiny::plotOutput("frame_cumsum", height = "400px", width = "100%")
          )
        )
      )
    ),

    server = function(input, output, session) {
      img_width <- shiny::reactiveVal(1000)
      first_frame <- shiny::reactive({
        shiny::req(frame_paths())
        tiff::readTIFF(frame_paths()[1], as.is = TRUE)
      })

      shiny::observe({
        shiny::req(first_frame())
        img <- first_frame()
        img_width(dim(img)[2])
        shiny::updateSliderInput(session, "chamber_split", max = img_width(), value = round(img_width()/2))
      })

      # ---- Render Frame ----
      output$frame_plot2 <- shiny::renderPlot({
        shiny::req(df_analysis(), first_frame())
        img <- first_frame()
        h <- dim(img)[1]; w <- dim(img)[2]

        graphics::par(xaxs = "i", yaxs = "i")
        graphics::plot(NA, xlim = c(0, w), ylim = c(h, 0), xlab = "", ylab = "", axes = FALSE, asp = 1)
        graphics::rasterImage(img, 0, h, w, 0, interpolate = FALSE)
        graphics::abline(v = input$chamber_split, col = "blue", lwd = 2, lty = 2)
      }, res = 96)

      # ---- Cumulative sum ----
      output$frame_cumsum <- shiny::renderPlot({
        shiny::req(df_analysis())
        df <- df_analysis()
        if(!all(c("frame_num", "id") %in% base::names(df))) {
          shiny::showNotification("Analysis hasn't been run yet, so export panel is not ready", type = "warning")
        } else {
          first <- stats::aggregate(frame_num ~ id, data = df, FUN = min)
          df_first <- base::merge(first, df, by = c("id", "frame_num"))

          vline <- input$chamber_split
          df_first$side <- base::ifelse((df_first$x1 + df_first$x2 + df_first$x3 + df_first$x4)/4 <= vline, "Left", "Right")

          start_dt <- lubridate::ymd_hm(base::paste(input$start_date, input$start_time))
          df_first$datetime <- start_dt + lubridate::minutes((df_first$frame_num - base::min(df$frame_num)) * 5)

          df_cum <- df_first |>
            dplyr::arrange(datetime) |>
            dplyr::group_by(side) |>
            dplyr::mutate(cumN = dplyr::row_number())

          ggplot2::ggplot(df_cum, ggplot2::aes(x = datetime, y = cumN, colour = side)) +
            ggplot2::geom_step(linewidth = 1.2) +
            ggplot2::labs(x = "Time", y = "Cumulative pupariations", colour = "Side") +
            ggplot2::theme_minimal(base_size = 14)
        }
      })

      # ---- Excel save path ----
      Excel_save_path <- shiny::reactive({
        shiny::req(frame_paths())
        base_file <- base::basename(frame_paths()[1])
        base_name <- tools::file_path_sans_ext(base_file)
        base_name <- base::gsub("\\..*$", "", base_name)
        candidate <- base::file.path("flySurvivalApp_output", paste0(base_name, "_export.xlsx"))
        counter <- 1
        while (base::file.exists(candidate)) {
          candidate <- base::file.path("flySurvivalApp_output", paste0(base_name, "_export_", counter, ".xlsx"))
          counter <- counter + 1
        }
        candidate
      })

      # ---- Export Excel ----
      shiny::observeEvent(input$export_excel, {
        shiny::req(df_analysis())
        df <- df_analysis()
        first_appearance <- stats::aggregate(frame_num ~ id, data = df, FUN = min)
        df_first <- base::merge(first_appearance, df, by = c("id", "frame_num"))

        vline_x <- input$chamber_split
        df_first$side <- base::ifelse((df_first$x1 + df_first$x2 + df_first$x3 + df_first$x4)/4 <= vline_x, "Left", "Right")
        df_first$Treatment <- base::ifelse(df_first$side == "Left", input$left_treatment, input$right_treatment)
        df_first$Dose <- base::ifelse(df_first$side == "Left", input$left_dose, input$right_dose)
        df_first$Dose_unit <- base::ifelse(df_first$side == "Left", input$left_dose_unit, input$right_dose_unit)
        df_first$Genotype <- base::ifelse(df_first$side == "Left", input$left_genotype, input$right_genotype)
        df_first$Replicate <- base::ifelse(df_first$side == "Left", input$left_replicate, input$right_replicate)

        start_datetime <- lubridate::ymd_hm(base::paste(input$start_date, input$start_time))
        df_first$datetime <- start_datetime + lubridate::minutes((df_first$frame_num - base::min(df$frame_num)) * 5)
        df_first$Date <- base::format(df_first$datetime, "%d.%m.%Y")
        df_first$Time <- base::format(df_first$datetime, "%H:%M")
        df_first$Event <- 1

        left_table <- df_first[df_first$side == "Left", c("Date","Time","Event","Treatment","Dose","Dose_unit","Genotype","Replicate")]
        right_table <- df_first[df_first$side == "Right", c("Date","Time","Event","Treatment","Dose","Dose_unit","Genotype","Replicate")]

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Left")
        openxlsx::addWorksheet(wb, "Right")
        openxlsx::writeData(wb, "Left", left_table)
        openxlsx::writeData(wb, "Right", right_table)
        openxlsx::saveWorkbook(wb, Excel_save_path(), overwrite = TRUE)

        shiny::showNotification("Excel exported.", type = "message")
      })
    }
  )
}
