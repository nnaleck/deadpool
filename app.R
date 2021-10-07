library(shiny)
library(ggplot2)
library(reshape2)
library(readxl)
library(bslib)
library(ROCR)
library(class)
library(dplyr)
library(magrittr)

# Reading data 
thematic::thematic_shiny(font = "auto")
df = read.csv("student-mat.csv")
dict = read_excel("data_dictionary.xlsx")

#Categorical cols
categorical = c('school', 'sex', 'age', 'address', 'famsize', 'Pstatus', 'Medu', 'Fedu',
                'Mjob', 'Fjob', 'reason', 'guardian', 'traveltime', 'studytime',
                'failures', 'schoolsup', 'famsup', 'paid', 'activities', 'nursery',
                'higher', 'internet', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc',
                'Walc', 'health')

quan = setdiff(names(df), categorical)

for (col in categorical){
    df[, col] <- as.factor(df[, col])    
}

#discretiser = function(x){
#    if (x<=10){
#        return("Tenure_0-10")
#    }else if (x<=20){
#        return("Tenure_10-20")
#    }else if (x<=30){
#        return("Tenure_20-30")
#    }else {
#        return("Tenure_30-40")
#    }
#}

# UI
ui <- fluidPage( theme = bs_theme(bootswatch = "flatly", base_font = font_google("PT Serif", local = TRUE)),
            navbarPage("Student Grade Prediction",
                
                tabPanel("Descriptive analysis",
                     h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                        tabsetPanel(
                            tabPanel("Résumé",
                                     fluidRow(
                                         column(6, 
                                                h3("dataset: Student grade prediction"),
                                                textOutput(outputId = "resumeIt")
                                                ),
                                         column(6, 
                                                tableOutput(outputId = "dict"))
                                     )),
                            tabPanel("Analyse univariée",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput("select", label = h4("select a feature for univariate analysis"),
                                                         choices = sort(names(df)),
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
                                                         choices = names(df),
                                                         selected=1
                                             ),
                                             selectInput("selectB", label="Second feature",
                                                         choices = sort(names(df)), 
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
            tabPanel("Variables impact on final Grade",
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
                                          )
                                      )
                                  )
                         ),
                         
                         tabPanel("Quantitave variables", 
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput("selectquant", label = h4("select a quantitative variable"),
                                                      choices = quan,
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
    
    output$table <- renderDataTable({df})
    
    tabStatsQual <- reactive({
        table.tmp <- as.data.frame(table(df[, input$select]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        table.tmp <- cbind(table.tmp, 
                           table.tmp[[2]]/nrow(df)*100,
                           table.tmp[[3]]/nrow(df)*100)
        colnames(table.tmp) <- c(input$select, "Effectifs", "Effectifs Cum.",
                                 "Fréquences", "Fréquences Cum.")
        table.tmp
    })
    
    # Tableau statistique [quantitative]
    tabStatsQuant <- reactive({
        q = data.frame(statistiques = c('min', 'quantile 25%', 'median', 'quantile 75%',
                                        'max', 'moyenne', 'ecart type'),
                       values = c(quantile(df[, input$select]), 
                                  mean(df[, input$select]),
                                  sd(df[, input$select]))
        )
        
    });
    
    output$dict <- renderTable({
        dict
    })
    
    output$resumeIt <- renderPrint({
        "This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school-related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful (see paper source for more details)."
    })
    
    output$statsTableOut <- renderTable({ 
        if (is.numeric(df[, input$select]))
        {
            tabStatsQuant()
        } else {
            tabStatsQual() 
        }
    });
    
    output$effectifsHist <- renderPlot({
        if (is.numeric(df[, input$select])){
            boxplot(df[, input$select], main=paste("Boxplot de", input$select, sep=" "))
            } else {
            effectifs <- table(df[, input$select])
            barplot(effectifs, ylab="Effectifs",
                    main = paste("diagramme en baton de ", input$select, sep=""))
            
        }
    });
    
    output$boxplotC <- renderPlot({
        ggplot(data=df)+
            geom_boxplot(mapping = aes_string("Attrition", input$selectquant))+
            xlab(label = "Churn")+
            ylab(label=input$selectquant)
    });
    
    output$frequenceHist <- renderPlot({
        if (is.numeric(df[, input$select]))
        {
            hist( df[, input$select], freq = FALSE,
                  main = paste("Histogramme de ", input$select, sep=""), col = "green",
                  xlab = input$select, ylab = "Densité de frequences", 
                  right = FALSE,)
        } else {
            # Calcul des effectifs
            effectifs <- table(df[, input$select])
            #Diagramme en secteur
            pie(effectifs, 
                main =paste("Diagramme en secteurs de ", input$select, sep=""))
        }
    });
    
    # Histogramme des frequences
    output$frequenceHistC <- renderPlot({
        ggplot(data=df)+
            geom_histogram(mapping = aes_string(input$selectquant, fill="Attrition"), bins = 10)+
            xlab(label = input$selectquant)+
            ylab(label="Frequency")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
    });
    
    # Courbe cumulative
    output$effectifsCumCurve <- renderPlot({
        if(! is.numeric(df[, input$select])) return(NULL)
        
        #Recuperation des donnees a partir de l'histogramme
        tmp.hist <- hist( df[, input$select], plot = FALSE,
                          right = FALSE)
        
        plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
             xlab = input$select,
             ylab = "Effectifs cumules",
             main = paste("Courbe cumulative de ", input$select, sep=""),
             type = "o", col = "blue", lwd = 2)
    });
    
    output$effectifsCumCurveC <- renderPlot({
        if(! is.numeric(df[, input$selectquant])) return(NULL)
        
        churn=c("Yes", "No")
        #Recuperation des donnees a partir de l'histogramme
        tmp.hist <- hist( df[df$Attrition=='Yes', ][, input$selectquant], plot = FALSE,
                              right = FALSE)
            
            plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
             xlab = input$selectquant,
             ylab = "Effectifs cumules",
             main = paste("Courbe cumulative de ", input$selectquant, sep=""),
             type = "o", col = "blue")
        tmp.hist1 <- hist( df[df$Attrition=='No', ][, input$selectquant], plot = FALSE,
                              right = FALSE)
            
        lines(x = tmp.hist1$breaks[-1], y = cumsum(tmp.hist1$counts), col = "red", type = "b")
        legend("topleft",
               c("Churn","Not churn"),
               fill=c("blue","red")
        )    
    });
    
    # tabStat
    output$tabStat <- renderTable({
        if(! is.numeric(df[, input$select])) return(NULL)
        tabStatsQuant()
    });
    
    ##Analyse bivariée: 
    output$nuagePointsBiv <- renderPlot({
        if (!is.numeric(df[, input$selectA]) & !is.numeric(df[, input$selectB])){
            return(ggplot(df, aes_string(x = input$selectA , fill=input$selectB)) + 
                       geom_bar() +
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if( is.numeric(df[, input$selectA]) &  is.numeric(df[, input$selectB])){
            plot(
                x = df[, input$selectA], y = df[, input$selectB],
                col = "red",
                main=paste(input$selectB, 'en fonction de ', input$selectA),
                xlab = input$selectA, ylab=input$selectB
            )
            abline(lm(df[, input$selectB]~df[, input$selectA]), col="blue", lwd = 2)
        }
        if(is.numeric(df[, input$selectA]) &  !is.numeric(df[, input$selectB])){
            return(ggplot(data=df)+
                       geom_histogram(mapping = aes_string(input$selectA, fill=input$selectB), bins = 10)+
                       xlab(label = input$selectA)+
                       ylab(label="Frequency")+
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if(!is.numeric(df[, input$selectA]) &  is.numeric(df[, input$selectB])){ 
            ggplot(data=df)+
                geom_histogram(mapping = aes_string(input$selectB, fill=input$selectA), bins = 10)+
                xlab(label = input$selectB)+
                ylab(label="Frequency")+
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        }
    })
    
    output$correlation <- renderText({
        if(! is.numeric(df[, input$selectA]) || ! is.numeric(df[, input$selectB])) return(NULL) 
        
        coeff_correlation.tmp <- cov(df[, input$selectA], df[, input$selectB])/(sqrt(var(df[, input$selectA])*var(df[, input$selectB])))
        paste('Coeff de corrélation linéaire = ', round(coeff_correlation.tmp, digits=2))
    })
    
    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(df, is.numeric))
        corrMatrix.tmp = round(cor(df[, nums.tmp]), 2)
        
        heatmap(corrMatrix.tmp)
    })
    output$histogrammeMod = renderPlot({
        if( is.numeric(df[, input$selectA]) & is.numeric(df[, input$selectB])){  
            columns = c(input$selectA, input$selectB)
            # Reshape data()
            data.stack <- melt(df[, columns], measure.vars = columns)
            # Boxplot élaborée
            return(qplot(x = data.stack[,1], y = data.stack[,2], 
                         xlab = "Modalités", ylab = "Mesures",
                         geom=c("boxplot"), fill=data.stack[,1]) +
                       theme(legend.title=element_blank()))
        }
        if ( is.numeric(df[, input$selectA]) & !is.numeric(df[, input$selectB])){
            
            return(qplot(x = df[, input$selectB], y = df[, input$selectA],
                         xlab = "Modalités", ylab = "Mesures",
                         geom=c("boxplot"), fill=df[, input$selectB]) +
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if ( !is.numeric(df[, input$selectA]) & is.numeric(df[, input$selectB])){
            ggplot(data=df)+
                geom_boxplot(mapping = aes_string(input$selectA, input$selectB))+
                xlab(label = input$selectA)+
                ylab(label=input$selectB)+
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        }
        
    })
    
  
    
    output$piechartYes = renderPlot({
        return(ggplot(data=df)+
                   geom_histogram(mapping = aes_string('G3', fill=input$selectqual), bins = 10)+
                   xlab(label = 'Final grade')+
                   ylab(label="Frequency")+
                   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
    })
    
    output$piechartNo = renderPlot({
        return(qplot(x = df[, input$selectqual], y = df[, "G3"],
                     xlab = paste("Modalités de", input$selectqual, sep=" "), ylab = "Final grade",
                     geom=c("boxplot","jitter"), fill= df[, input$selectqual]) +
                   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
    })
    
      
    output$yearsNum = renderPlot({
        df_tmp = data.frame(df)
        tenure = sapply(df$YearsAtCompany, discretiser)
        df_tmp$Tenure = tenure
        return(ggplot(df_tmp, aes_string(x = 'Tenure' , fill='Attrition')) + 
                   geom_bar() +
                   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                   labs(
                       title= paste("Tenure groups count"))
                   )
    })
    
    output$yearsInc = renderPlot({
        df_tmp = data.frame(df)
        tenure = sapply(df$YearsAtCompany, discretiser)
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
