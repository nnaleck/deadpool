# Loading project files
source('bootstrap.R')
source('ui.R')
source('server.R')

# Run the application 
shinyApp(
    ui = deadpoolUI, server = deadpoolServer
)
