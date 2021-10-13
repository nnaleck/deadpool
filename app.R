# Loading project files
source('ui.R')
source('server.R')

# Run the application 
shinyApp(
    ui = deadpoolUI, server = deadpoolServer
)
