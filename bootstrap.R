# Loading libraries
library(shiny)
library(reshape2)
library(ROCR)
library(class)
library(dplyr)
library(readxl)
library(ROCR)
library(pROC)
library(class)
library(dplyr)
library(magrittr)
library(factoextra)
library(ggbiplot)
library(plotly)
library(bslib)

# Using custom shiny theme
thematic::thematic_shiny(font = "auto")

# Defining categorical variables
categorical <- c('school', 'sex', 'age', 'address', 'famsize', 'Pstatus', 'Medu', 'Fedu',
                 'Mjob', 'Fjob', 'reason', 'guardian', 'traveltime', 'studytime',
                 'failures', 'schoolsup', 'famsup', 'paid', 'activities', 'nursery',
                 'higher', 'internet', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc',
                 'Walc', 'health')

# Reading dataset(s)
df <- read.csv("datasets/student-mat.csv")
df_encoded <- read.csv("datasets/df_encoded.csv")
dict <- read_excel("datasets/data_dictionary.xlsx")

# Factorizing categorical variables
for (col in categorical){
    df[, col] <- as.factor(df[, col])
}
    
# Defining quantitative variables
quantitative <- setdiff(names(df), categorical)

# Helper functions
discretize <- function(x) {
    start <- 0
    end <- 4

    while(TRUE)
    {
        if (x < end)
        {
            name <- paste('Absence[', start)
            name <- paste(name, end, sep=',')
            name <- paste(name, ']')
            

            return(name)
        }

        start <- start + 4
        end <- end + 4
    }
}

discretizeGrades <- function(x)
{
    if (x<10) return("Echoue") else return("Valide")
}

binarize <- function(x)
{
    if (x<10) return("0") else return("1")
}

