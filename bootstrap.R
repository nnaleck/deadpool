# Using custom shiny theme
thematic::thematic_shiny(font = "auto")

# Defining categorical variables
categorical <- c('school', 'sex', 'age', 'address', 'famsize', 'Pstatus', 'Medu', 'Fedu',
                 'Mjob', 'Fjob', 'reason', 'guardian', 'traveltime', 'studytime',
                 'failures', 'schoolsup', 'famsup', 'paid', 'activities', 'nursery',
                 'higher', 'internet', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc',
                 'Walc', 'health')

# Reading dataset(s)
df <- read.csv("student-mat.csv")
df_encoded <- read.csv("df_encoded.csv")
dict <- read_excel("data_dictionary.xlsx")

# Factorizing categorical variables
for (col in categorical){
    df[, col] <- as.factor(df[, col])
}
    
# Defining quantitative variables
quantitative <- setdiff(names(df), categorical)

# Helper functions
discretize <- function(x) {
    start <- 0
    end <- 2

    while(TRUE)
    {
        if (x <= end)
        {
            name <- paste('Tenure_', start)
            name <- paste(name, '-')
            name <- paste(name, end)

            return(name)
        }

        start <- start + 2
        end <- end + 2
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

