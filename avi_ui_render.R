library(shiny)
library(shinyFiles)

# Example function
# source("ffmpeg_script.R")

ui <- fluidPage(
  titlePanel("Run Function on a Folder"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("dir", "Choose a folder", "Select a folder from your PC"),
      actionButton("run_btn", "Run Function", class = "btn-primary")
    ),
    
    mainPanel(
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {
  
  if (.Platform$OS.type == "windows") {
    drives <- paste0(LETTERS, ":/")
    drives <- drives[sapply(drives, function(x) suppressWarnings(file.exists(x)))]
    roots <- setNames(drives, gsub(":/", "", drives))  # e.g. "C" = "C:/", "D" = "D:/"
  } else {
    roots <- c("root" = "/")
  }

  shinyDirChoose(input, "dir", roots = roots, session = session)
  
  observeEvent(input$run_btn, {
    req(input$dir)  # ensure folder is selected
    
    # Convert to full path
    folder_path <- parseDirPath(roots, input$dir)
    
    # Run your function (dummy for now)
    res <- paste("Pretend Avi_to_tiff ran on", folder_path)
    
    output$result <- renderText({
      paste("Function ran on:", folder_path, "\n\nResult:\n", res)
    })
  })
}

shinyApp(ui, server)
