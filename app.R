# Loading libraries
library(shiny)
library(ggplot2)
library(reshape2)
library(readxl)
library(bslib)
library(ROCR)
library(pROC)
library(class)
library(dplyr)
library(magrittr)
library(factoextra)
library(ggbiplot)
library(plotly)

# Loading project files
source('bootstrap.R')
source('ui.R', local = TRUE)
source('server.R')

# Run the application 
shinyApp(
    ui = deadpoolUI, server = deadpoolServer
)
