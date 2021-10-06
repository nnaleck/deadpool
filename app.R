library(shiny)
library(ggplot2)
library(reshape2)
library(readxl)
library(bslib)
library(ROCR)
library(class)
library(dplyr)
library(magrittr)

thematic::thematic_shiny(font = "auto")
dataset = read.csv("employee_attrition_1.csv")
df1 = read.csv("employee_attrition_2.csv")
df2 = read.csv("employee_attrition_2.csv")
dict = read_excel("data_dictionary.xlsx")
categorical = c('Education', 'EnvironmentSatisfaction', 'JobInvolvement', 'JobSatisfaction', 
                    'PerformanceRating', 'WorkLifeBalance', 'JobLevel', 'BusinessTravel', 'Department', 
                    'EducationField', 'Gender', 'JobRole', 'MaritalStatus', 'StockOptionLevel')
qual = setdiff(names(dataset), union(categorical, c("Attrition")))

for (col in categorical){
    dataset[, col] <- as.factor(dataset[, col])    
}

discretiser = function(x){
    if (x<=10){
        return("Tenure_0-10")
    }else if (x<=20){
        return("Tenure_10-20")
    }else if (x<=30){
        return("Tenure_20-30")
    }else {
        return("Tenure_30-40")
    }
}

# UI
ui <- fluidPage( theme = bs_theme(bootswatch = "flatly", base_font = font_google("PT Serif", local = TRUE)),
            navbarPage("Employee attrition",
                
                tabPanel("Presentation des donnees",
                     h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                        tabsetPanel(
                            tabPanel("Résumé",
                                     fluidRow(
                                         column(6, 
                                                h3("Dataset: Employee attrition"),
                                                tableOutput(outputId = "dict")),
                                         column(6, 
                                                plotOutput(outputId = "churnPortion"))
                                     )),
                            tabPanel("Analyse univariée",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput("select", label = h4("select a feature for univariate analysis"),
                                                         choices = sort(names(dataset)),
                                                         selected=1
                                             )
                                         ),
                                         mainPanel(
                                             fluidRow(
                                                 column(6, 
                                                        plotOutput(outputId = "effectifsHist")),
                                                 column(6, 
                                                        plotOutput(outputId = "frequenceHist"))
                                             ),
                                             fluidRow(
                                                 column(6, 
                                                        plotOutput(outputId = "effectifsCumCurve")),
                                                 column(6, 
                                                        tableOutput(outputId = "tabStat"))
                                             ) 
                                         )
                                     )
                            ),
                            tabPanel("Analyse bivariée",
                                     sidebarLayout(
                                         sidebarPanel(
                                             h4("Select 2 features for bivariate analysis"),
                                             selectInput("selectA", label = "First feature",
                                                         choices = names(dataset),
                                                         selected=1
                                             ),
                                             selectInput("selectB", label="Second feature",
                                                         choices = sort(names(dataset)), 
                                                         selected = 1)
                                         ),
                                         mainPanel(
                                             fluidRow(
                                                 column(6, fluidRow(
                                                     column(12, plotOutput(outputId = "nuagePointsBiv")),
                                                     column(4, offset = 3, textOutput("correlation"))
                                                 )),
                                                 column(6, 
                                                        plotOutput(outputId = "histogrammeMod"))
                                             ),
                                             fluidRow(
                                                 column(6, 
                                                        tableOutput(outputId = "contingence")),
                                                 column(6, 
                                                        plotOutput(outputId = "boxplotB"))
                                             ) 
                                         )
                                     )
                            ),
                            tabPanel("Table", dataTableOutput("table"), style = "font-size: 85%")
                        )
            ),
            tabPanel("Employees attrition",
                     h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                     tabsetPanel(
                         tabPanel("Qualitative variables",
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput("selectqual", label = h4("select a qualitative variable"),
                                                      choices = categorical,
                                                      selected=1
                                          )
                                      ),
                                      mainPanel(   
                                          fluidRow(
                                              column(6, 
                                                     plotOutput(outputId = "piechartYes")),
                                              column(6, 
                                                     plotOutput(outputId = "piechartNo"))
                                          ),
                                          fluidRow(
                                              column(6,
                                                     plotOutput(outputId= "barplotC"))
                                          )
                                      )
                                  )
                         ),
                         
                         tabPanel("Quantitave variables", 
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput("selectquant", label = h4("select a quantitative variable"),
                                                      choices = qual,
                                                      selected=1
                                          )
                                      ),
                                      mainPanel(   
                                          fluidRow(
                                              column(6, 
                                                     plotOutput(outputId = "boxplotC")),
                                              column(6, 
                                                     plotOutput(outputId = "frequenceHistC"))
                                          ),
                                          fluidRow(
                                              column(6, 
                                                     plotOutput(outputId = "effectifsCumCurveC")),
                                              column(6, 
                                                     tableOutput(outputId = "tabStatC"))
                                          )  
                                      )
                                  )
                        ),
                        tabPanel("Analyse selon 'YearsAtCompany'",
                                 fluidRow(
                                     column(6, 
                                            plotOutput(outputId = "yearsNum")),
                                     column(6, 
                                            plotOutput(outputId = "yearsInc"))
                                 )
                                 
                        )
                     )
            ),
            tabPanel("Apprentissage supervisé", 
                     h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                     tabsetPanel(
                         tabPanel("k nearest neighbors",
                                  sidebarLayout(
                                  sidebarPanel(
                                    sliderInput('k', 'Select the Number of Nearest Neighbours', value = 6, min = 1, max = 100),
                                    checkboxInput('balance', label = "Balance data (only for training)")
                                  ),
                                  mainPanel(
                                      fluidRow(
                                          column(6, 
                                                 plotOutput(outputId = "boxplotAcc")
                                                 ),
                                          column(6, 
                                                 plotOutput(outputId = "ROC")
                                                 )
                                          )   
                                      )   
                                  )
                         ),
                         tabPanel("Logistic regression",
                                  checkboxInput('balance2', label = "Balance data (only for training)"),
                                  fluidRow(
                                      column(6, 
                                             plotOutput(outputId = "boxplotAcc2")
                                      ),
                                      column(6, 
                                             plotOutput(outputId = "ROC2")
                                      )
                                  )   
                         )   
                         )
                         )
            )
        )



server <- function(input, output) {
    
    output$table <- renderDataTable({dataset})
    
    tabStatsQual <- reactive({
        table.tmp <- as.data.frame(table(dataset[, input$select]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        table.tmp <- cbind(table.tmp, 
                           table.tmp[[2]]/nrow(dataset)*100,
                           table.tmp[[3]]/nrow(dataset)*100)
        colnames(table.tmp) <- c(input$select, "Effectifs", "Effectifs Cum.",
                                 "Fréquences", "Fréquences Cum.")
        table.tmp
    })
    
    # Tableau statistique [quantitative]
    tabStatsQuant <- reactive({
        q = data.frame(statistiques = c('min', 'quantile 25%', 'median', 'quantile 75%',
                                        'max', 'moyenne', 'ecart type'),
                       values = c(quantile(dataset[, input$select]), 
                                  mean(dataset[, input$select]),
                                  sd(dataset[, input$select]))
        )
        
    });
    
    output$dict <- renderTable({
        dict
    })
    
    output$churnPortion <- renderPlot({
        effectifs <- table(dataset$Attrition)
        pie(effectifs, radius=1, 
            main ="Distribution de la variable \"Attrition\" ")
    })
    
    output$statsTableOut <- renderTable({ 
        if (is.numeric(dataset[, input$select]))
        {
            tabStatsQuant()
        } else {
            tabStatsQual() 
        }
    });
    
    output$effectifsHist <- renderPlot({
        if (is.numeric(dataset[, input$select])){
            boxplot(dataset[, input$select], main=paste("Boxplot de", input$select, sep=" "))
            } else {
            effectifs <- table(dataset[, input$select])
            barplot(effectifs, ylab="Effectifs",
                    main = paste("diagramme en baton de ", input$select, sep=""))
            
        }
    });
    
    output$boxplotC <- renderPlot({
        ggplot(data=dataset)+
            geom_boxplot(mapping = aes_string("Attrition", input$selectquant))+
            xlab(label = "Churn")+
            ylab(label=input$selectquant)
    });
    
    output$frequenceHist <- renderPlot({
        if (is.numeric(dataset[, input$select]))
        {
            hist( dataset[, input$select], freq = FALSE,
                  main = paste("Histogramme de ", input$select, sep=""), col = "green",
                  xlab = input$select, ylab = "Densité de frequences", 
                  right = FALSE,)
        } else {
            # Calcul des effectifs
            effectifs <- table(dataset[, input$select])
            #Diagramme en secteur
            pie(effectifs, 
                main =paste("Diagramme en secteurs de ", input$select, sep=""))
        }
    });
    
    # Histogramme des frequences
    output$frequenceHistC <- renderPlot({
        ggplot(data=dataset)+
            geom_histogram(mapping = aes_string(input$selectquant, fill="Attrition"), bins = 10)+
            xlab(label = input$selectquant)+
            ylab(label="Frequency")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
    });
    
    # Courbe cumulative
    output$effectifsCumCurve <- renderPlot({
        if(! is.numeric(dataset[, input$select])) return(NULL)
        
        #Recuperation des donnees a partir de l'histogramme
        tmp.hist <- hist( dataset[, input$select], plot = FALSE,
                          right = FALSE)
        
        plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
             xlab = input$select,
             ylab = "Effectifs cumules",
             main = paste("Courbe cumulative de ", input$select, sep=""),
             type = "o", col = "blue", lwd = 2)
    });
    
    output$effectifsCumCurveC <- renderPlot({
        if(! is.numeric(dataset[, input$selectquant])) return(NULL)
        
        churn=c("Yes", "No")
        #Recuperation des donnees a partir de l'histogramme
        tmp.hist <- hist( dataset[dataset$Attrition=='Yes', ][, input$selectquant], plot = FALSE,
                              right = FALSE)
            
            plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
             xlab = input$selectquant,
             ylab = "Effectifs cumules",
             main = paste("Courbe cumulative de ", input$selectquant, sep=""),
             type = "o", col = "blue")
        tmp.hist1 <- hist( dataset[dataset$Attrition=='No', ][, input$selectquant], plot = FALSE,
                              right = FALSE)
            
        lines(x = tmp.hist1$breaks[-1], y = cumsum(tmp.hist1$counts), col = "red", type = "b")
        legend("topleft",
               c("Churn","Not churn"),
               fill=c("blue","red")
        )    
    });
    
    # tabStat
    output$tabStat <- renderTable({
        if(! is.numeric(dataset[, input$select])) return(NULL)
        tabStatsQuant()
    });
    
    ##Analyse bivariée: 
    output$nuagePointsBiv <- renderPlot({
        if (!is.numeric(dataset[, input$selectA]) & !is.numeric(dataset[, input$selectB])){
            return(ggplot(dataset, aes_string(x = input$selectA , fill=input$selectB)) + 
                       geom_bar() +
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if( is.numeric(dataset[, input$selectA]) &  is.numeric(dataset[, input$selectB])){
            plot(
                x = dataset[, input$selectA], y = dataset[, input$selectB],
                col = "red",
                main=paste(input$selectB, 'en fonction de ', input$selectA),
                xlab = input$selectA, ylab=input$selectB
            )
            abline(lm(dataset[, input$selectB]~dataset[, input$selectA]), col="blue", lwd = 2)
        }
        if(is.numeric(dataset[, input$selectA]) &  !is.numeric(dataset[, input$selectB])){
            return(ggplot(data=dataset)+
                       geom_histogram(mapping = aes_string(input$selectA, fill=input$selectB), bins = 10)+
                       xlab(label = input$selectA)+
                       ylab(label="Frequency")+
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if(!is.numeric(dataset[, input$selectA]) &  is.numeric(dataset[, input$selectB])){ 
            ggplot(data=dataset)+
                geom_histogram(mapping = aes_string(input$selectB, fill=input$selectA), bins = 10)+
                xlab(label = input$selectB)+
                ylab(label="Frequency")+
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        }
    })
    
    output$correlation <- renderText({
        if(! is.numeric(dataset[, input$selectA]) || ! is.numeric(dataset[, input$selectB])) return(NULL) 
        
        coeff_correlation.tmp <- cov(dataset[, input$selectA], dataset[, input$selectB])/(sqrt(var(dataset[, input$selectA])*var(dataset[, input$selectB])))
        paste('Coeff de corrélation linéaire = ', round(coeff_correlation.tmp, digits=2))
    })
    
    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(dataset, is.numeric))
        corrMatrix.tmp = round(cor(dataset[, nums.tmp]), 2)
        
        heatmap(corrMatrix.tmp)
    })
    output$histogrammeMod = renderPlot({
        if( is.numeric(dataset[, input$selectA]) & is.numeric(dataset[, input$selectB])){  
            columns = c(input$selectA, input$selectB)
            # Reshape data()
            data.stack <- melt(dataset[, columns], measure.vars = columns)
            # Boxplot élaborée
            return(qplot(x = data.stack[,1], y = data.stack[,2], 
                         xlab = "Modalités", ylab = "Mesures",
                         geom=c("boxplot"), fill=data.stack[,1]) +
                       theme(legend.title=element_blank()))
        }
        if ( is.numeric(dataset[, input$selectA]) & !is.numeric(dataset[, input$selectB])){
            
            return(qplot(x = dataset[, input$selectB], y = dataset[, input$selectA],
                         xlab = "Modalités", ylab = "Mesures",
                         geom=c("boxplot"), fill=dataset[, input$selectB]) +
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if ( !is.numeric(dataset[, input$selectA]) & is.numeric(dataset[, input$selectB])){
            ggplot(data=dataset)+
                geom_boxplot(mapping = aes_string(input$selectA, input$selectB))+
                xlab(label = input$selectA)+
                ylab(label=input$selectB)+
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        }
        
    })
    
    output$barplotC = renderPlot({
        ggplot(dataset, aes_string(x = input$selectqual , fill="Attrition")) + 
            geom_bar() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    })
    
    output$piechartYes = renderPlot({
        effectifs <- table(dataset[dataset$Attrition=="Yes",][, input$selectqual])
        pie(effectifs, 
            main = paste(input$selectqual, " avec Attrition=Yes", sep=""))
    })
    
    output$piechartNo = renderPlot({
        effectifs <- table(dataset[dataset$Attrition=="No",][, input$selectqual])
        pie(effectifs, 
            main = paste(input$selectqual, " avec Attrition=No", sep=""))
    })
    
    output$boxplotAcc = renderPlot({
        B <- 10
        acc_valid <- rep(NA,10)
        
        for (b in 1:B)
        {
            smp_size <- floor(0.75 * nrow(df1))
            tr <- sample(1:nrow(df1),smp_size)
            
            if (input$balance){
                df = df2    
            }else{
                df = df1
            }
            
            train <- df[tr,]
            test <- df[-tr,]
            ka = input$k
            pred <- knn(train[, -7],test[, -7],train$Attrition,k=ka)
            acc_valid[b] <- mean(pred==test$Attrition)
        }
        boxplot(acc_valid,main="Accuracy lors des 10-fold cross validation")
    })
    
    output$ROC = renderPlot({
        smp_size <- floor(0.75 * nrow(df1))
        tr <- sample(1:nrow(df1),smp_size)
        
        if (input$balance){
            df = df2    
        }else{
            df = df1
        }
        
        train <- df[tr,]
        test <- df[-tr,]
        prob <- rep(NA, nrow(test))
        ka = input$k
        res <- knn(train[, -7],test[, -7],train$Attrition,k=ka, prob=TRUE)
        prob[res==1] <- attr(res,"prob")[res==1]
        prob[res==0] <- 1-attr(res,"prob")[res==0]
        
        pred <- prediction(prob, test$Attrition)
        perf <- performance(pred, "tpr", "fpr")
        plot(perf, main="Courbe ROC") #courbe ROC
        abline(a=0, b=1)
        
    })
    output$boxplotAcc2 = renderPlot({
        B <- 10
        acc_valid <- rep(NA,10)
        
        for (b in 1:10)
        {
            smp_size <- floor(0.75 * nrow(df1))
            tr <- sample(1:nrow(df1),smp_size)
            
            if (input$balance2){
                df = df2    
            }else{
                df = df1
            }
            
            train <- df[tr,]
            test <- df[-tr,]
            
            # Fit the model
            model <- glm( Attrition ~., data = train, family = binomial)
            prob <- model %>% predict(test[, -7], type = "response")
            pred <- ifelse(prob > 0.5, 1, 0)
            acc_valid[b] <- mean(pred==test$Attrition)
        }
        boxplot(acc_valid,main="Accuracy lors des 10-fold cross validation")
    })
    
    output$ROC2 = renderPlot({
        smp_size <- floor(0.75 * nrow(df1))
        tr <- sample(1:nrow(df1),smp_size)
        
        if (input$balance2){
            df = df2    
        }else{
            df = df1
        }
        
        train <- df[tr,]
        test <- df[-tr,]
        # Fit the model
        model <- glm( Attrition ~., data = train, family = binomial)
        prob <- model %>% predict(test[, -7], type = "response")
        
        pred <- prediction(prob, test$Attrition, label.ordering=c("0","1"))
        perf <- performance(pred, "tpr", "fpr")
        plot(perf, main="Courbe ROC") #courbe ROC
        abline(a=0, b=1)
        
    })
    
    output$yearsNum = renderPlot({
        df_tmp = data.frame(dataset)
        tenure = sapply(dataset$YearsAtCompany, discretiser)
        df_tmp$Tenure = tenure
        return(ggplot(df_tmp, aes_string(x = 'Tenure' , fill='Attrition')) + 
                   geom_bar() +
                   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                   labs(
                       title= paste("Tenure groups count"))
                   )
    })
    
    output$yearsInc = renderPlot({
        df_tmp = data.frame(dataset)
        tenure = sapply(dataset$YearsAtCompany, discretiser)
        df_tmp$Tenure = tenure
        df_tmp %>%
            group_by(Tenure, Attrition) %>%
            summarise(mean_monthly_inc = mean(MonthlyIncome)) %>%
            ggplot(aes(x = Tenure, y = mean_monthly_inc, fill = Attrition)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(
                x = "Tenure groups",
                y = "Average monthly income",
                title = paste(
                    "Average monthly income by tenure groups"
                )
            )
             
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
