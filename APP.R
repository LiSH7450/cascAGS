# Import UI definitions from the ui.R script
source("ui.R")

# Import server logic from the server.R script
source("server.R")

# Create and run the Shiny app
shinyApp(ui = ui, server = server)

