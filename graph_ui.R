graph_panel <- function() {
  list(
    ui = tabPanel(
      "Graph inferences",
      fluidPage(
        titlePanel("Analyze Shapes from Stats File"),
        
        sidebarLayout(
          sidebarPanel(
            fileInput("stats_file", "Upload Stats File (.txt or .csv)", accept = c(".txt", ".csv")),
            numericInput("threshold_ratio", "Merging Threshold Ratio:", 0.12, min = 0.01, max = 1, step = 0.01),
            numericInput("min_appearances", "Minimum Appearances per ID:", 2, min = 1, step = 1),
            numericInput("frame_min", "Pupariation Cut-off (Min Frame):", NA, min = 1, step = 1),
            numericInput("frame_max", "Fly Emergence Cut-off (Max Frame):", NA, min = 1, step = 1),
            actionButton("run_analysis", "Run Analysis", class = "btn-success")
          ),
          mainPanel(
            h4("Pupariation Curve"),
            plotOutput("shapes_plot")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      df_processed <- eventReactive(input$run_analysis, {
        req(input$stats_file)
        
        # --- Load file ---
        df <- read.table(input$stats_file$datapath, header = TRUE, sep = "\t")
        
        # --- Convert frame names to numeric indices ---
        df$frame_num <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", df$frame))
        
        # --- Construct point list (flatten x1..y4) ---
        df$points <- apply(df[, c("x1","y1","x2","y2","x3","y3","x4","y4")], 1, as.numeric, simplify = FALSE)
        
        # --- Merging logic ---
        threshold_ratio <- input$threshold_ratio
        df$id <- NA_integer_
        next_id <- 1
        
        for (i in seq_len(nrow(df))) {
          if (is.na(df$id[i])) {
            df$id[i] <- next_id
            current_points <- unlist(df$points[i])
            for (j in (i+1):nrow(df)) {
              if (is.na(df$id[j])) {
                compare_points <- unlist(df$points[j])
                distance <- sqrt(sum((current_points - compare_points)^2))
                norm <- sqrt(sum(current_points^2))
                if (distance / norm < threshold_ratio) {
                  df$id[j] <- df$id[i]
                }
              }
            }
            next_id <- next_id + 1
          }
        }
        
        # --- Remove low-frequency IDs ---
        id_counts <- table(df$id)
        keep_ids <- names(id_counts[id_counts >= input$min_appearances])
        df <- df[df$id %in% keep_ids, ]
        
        # --- Apply frame cut-offs ---
        if (!is.na(input$frame_min)) {
          df <- df[df$frame_num >= input$frame_min, ]
        }
        if (!is.na(input$frame_max)) {
          df <- df[df$frame_num <= input$frame_max, ]
        }
        
        df
      })
      
      output$shapes_plot <- renderPlot({
        req(df_processed())
        library(ggplot2)
        df <- df_processed()
        
        # count shapes per frame number
        shape_counts <- aggregate(id ~ frame_num, data = df, FUN = length)
        colnames(shape_counts) <- c("frame_num", "count")
        
        ggplot(shape_counts, aes(x = frame_num, y = count)) +
          geom_point(color = "blue") +
          geom_smooth(method = "loess", se = FALSE, color = "red", group = 1) +
          labs(x = "Frame Number", y = "Number of Shapes",
               title = "Pupariation Curve (Shapes per Frame)") +
          theme_minimal()
      })
    }
  )
}
