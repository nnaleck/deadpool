# Loading files containing application logic
source('logic/univariate.R')
source('logic/bivariate.R')

# Factorizing categorical variables
for (col in categorical){
    df[, col] <- as.factor(df[, col])
}

# Defining categorical variables
categorical <- c('school', 'sex', 'age', 'address', 'famsize', 'Pstatus', 'Medu', 'Fedu',
                 'Mjob', 'Fjob', 'reason', 'guardian', 'traveltime', 'studytime',
                 'failures', 'schoolsup', 'famsup', 'paid', 'activities', 'nursery',
                 'higher', 'internet', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc',
                 'Walc', 'health')

# Defining quantitative variables
quantitative <- setdiff(names(df), categorical)

deadpoolServer <- function(input, output) {
    output$table <- renderDataTable({df})

    qualitativeTabStats <- reactive({
        table.tmp <- as.data.frame(table(df[, input$univariateSelect]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        table.tmp <- cbind(table.tmp,
                           table.tmp[[2]]/nrow(df)*100,
                           table.tmp[[3]]/nrow(df)*100)
        colnames(table.tmp) <- c(input$univariateSelect, "Effectifs", "Effectifs Cum.",
                                 "Fréquences", "Fréquences Cum.")
        table.tmp
    })

    # Quantitative statistic table
    quantitativeTabStats <- reactive({
        data.frame(
            stat = c('min', 'quantile 25%', 'median', 'quantile 75%', 'max', 'avg', 'standard dev.'),
            values = c(quantile(df[, input$univariateSelect]), mean(df[, input$univariateSelect]), sd(df[, input$univariateSelect]))
        )
    })

    output$dictionary <- renderTable({
        dict
    })
    
    output$datasetSummary <- renderPrint({
        "This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school-related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful (see paper source for more details)."
    })

    output$statsTableOut <- renderTable({
        if (is.numeric(df[, input$univariateSelect]))
        {
            quantitativeTabStats()
        }
        else
        {
            qualitativeTabStats()
        }
    })

    # Occurences (effectifs) plot for univariate analyzis
    # Depending on the variable type, it is either a boxplot or a barplot
    output$occurencesPlot <- renderPlotly({
        plotOccurences(input$univariateSelect)
    })

    # Frequencies plot
    # Depending on variable type, it is either a barplot or a pie chart
    output$frequencyPlot <- renderPlotly({
        plotFrequencies(input$univariateSelect)
    })

    # Cumulative occurences plot
    output$cumulativeOccurencesPlot <- renderPlotly({
        plotCumulativeOccurences(input$univariateSelect)
    })

    # Quantitative statistics summary
    output$tabStat <- renderTable({
        if(! is.numeric(df[, input$univariateSelect])) return(NULL)

        quantitativeTabStats()
    })

    # Bivariate cloud points
    output$bivariateCloudPoints <- renderPlotly({
        plotBivariateCloudPoints(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature
        )
    })

    # Correlation
    output$correlation <- renderText({
        computeCorrelation(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature
        )
    })

    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(df, is.numeric))
        corrMatrix.tmp = round(cor(df[, nums.tmp]), 2)

        heatmap(corrMatrix.tmp)
    })

    # Bivariate analysis boxplot plot
    output$bivariateBoxplot <- renderPlotly({
        bivariateBoxPlot(
            input$bivariateFirstFeature,
            input$bivariateSecondFeature
        )
    })

    # Histogram of a qualitative feature according to final grade.
    output$qualitativeHistogramG3 <- renderPlotly({
        p <- ggplot(data=df) +
            geom_histogram(
                mapping = aes_string('G3', fill=input$selectqual),
                bins = 10
            ) +
            xlab(label = 'Final grade') +
            ylab(label = "Frequency") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    })

    # Boxplot of a qualitative feature according to final grade.
    output$qualitativeBoxplotsG3 <- renderPlotly({
        p <- qplot(
            x = df[, input$selectqual],
            y = df[, "G3"],
            xlab = paste("Modalités de", input$selectqual, sep=" "),
            ylab = "Final grade",
            geom=c("boxplot","jitter"),
            fill= df[, input$selectqual]
        ) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    })

    # Cloud points of a quantitative feature according to final grade.
    output$quantitativeCloudPointsG3 <- renderPlotly({
        p <- plot_ly(
            x = df[, input$selectquant],
            y = df[, 'G3'],
            name = paste('Final grade according to', input$selectquant),
            type = 'scatter',
            mode = 'markers'
        )

        p <- p %>% add_trace(
            x = df[, input$selectquant],
            y = fitted(lm(df[, 'G3']~df[, input$selectquant])),
            mode = 'lines',
            name = 'Linear model'
        )

        return(p)
    })

    # Correlation factor of a quantitative feature according to final grade.
    output$quantitativeCorrG3 <- renderText({
        computeCorrelation('G3', input$selectquant)
    })

    # Boxplot of a quantitative feature according to final grade.
    output$quantitativeBoxplotsG3 <- renderPlotly({
        columns <- c('G3', input$selectquant)

        # Reshape data()
        data.stack <- melt(df[, columns], measure.vars = columns)

        p <- qplot(
            x = data.stack[,1],
            y = data.stack[,2],
            xlab = "Modalités",
            ylab = "Mesures",
            geom= "boxplot",
            fill=data.stack[, 1]
        ) + theme(legend.title=element_blank())

        return(ggplotly(p))
    })

    # Histogram of absences according to final grade.
    output$absenceHistogram <- renderPlotly({
        df_tmp <- data.frame(df)
        tenure <- sapply(df$absences, discretize)
        df_tmp$Tenure <- tenure

        p <- ggplot(
                df_tmp,
                aes_string(x = 'G3' , fill='Tenure')
            ) +
            geom_histogram() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            labs(title= paste("Tenure groups count"))

        return(ggplotly(p))
    })

    # Boxplots of absences according to final grade.
    output$absenceBoxplot <- renderPlotly({
        df_tmp <- data.frame(df)
        tenure <- sapply(df$absences, discretize)
        df_tmp$Tenure <- tenure

        p <- qplot(
            x = df_tmp$Tenure,
            y = df_tmp$G3,
            xlab = "Modalités",
            ylab = "Final grade(G3)",
            geom = "boxplot",
            fill=df_tmp$Tenure
        ) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    })

    # KNN Accuracy boxplot
    output$accuracyBoxplot <- renderPlotly({
        B <- 10
        acc_valid <- rep(NA,10)
        
        for (b in 1:B)
        {
            smp_size <- floor(0.75 * nrow(df_encoded))
            tr <- sample(seq_len(nrow(df_encoded)), smp_size)
            
            train <- df_encoded[tr,]
            trainClass <- sapply(train$G3, discretizeGrades)

            test <- df_encoded[-tr,]
            testClass <- sapply(test$G3, discretizeGrades)

            ka <- input$k
            
            pred <- knn(
                train[, -ncol(df_encoded)],
                test[, -ncol(df_encoded)],
                trainClass,
                k=ka
            )
            acc_valid[b] <- mean(pred==testClass)
        }
        
        fig <- plot_ly(y = acc_valid, type = 'box', name='accuracy (succeed or fail)')
        fig <- fig %>% layout(title = "KNN validation accuracy (10-fold CV)")

        return(fig)
    })

    # KNN ROC Curve plot.
    output$ROC <- renderPlot({
        smp_size <- floor(0.75 * nrow(df_encoded))
        tr <- sample(seq_len(nrow(df_encoded)), smp_size)

        train <- df_encoded[tr,]
        trainClass <- sapply(train$G3, binarize)

        test <- df_encoded[-tr,]
        testClass <- sapply(test$G3, binarize)

        prob <- rep(NA, nrow(test))
        ka <- input$k

        res <- knn(
            train[, -ncol(df_encoded)],
            test[, -ncol(df_encoded)],
            trainClass,
            k=ka,
            prob=TRUE
        )

        prob[res==1] <- attr(res,"prob")[res==1]
        prob[res==0] <- 1 - attr(res,"prob")[res==0]

        pred <- prediction(prob, testClass)
        perf <- performance(pred, "tpr", "fpr")

        plot(perf, main="ROC Curve")
        abline(a=0, b=1)
    })

    # output$boxplotAcc2 <- renderPlotly({
    #     B <- 10
    #     acc_valid <- rep(NA,10)
    #
    #     for (b in 1:10)
    #     {
    #         smp_size <- floor(0.75 * nrow(df))
    #         tr <- sample(1:nrow(df),smp_size)
    #
    #
    #         train <- df_encoded[tr,]
    #         trainClass = sapply(train$G3, binarize)
    #         test <- df_encoded[-tr,]
    #         testClass = sapply(test$G3, binarize)
    #         train = train[, -ncol(df_encoded)]
    #         train['G3'] = as.numeric(trainClass)
    #         # Fit the model
    #         model <- glm(  G3 ~., data = train, family = binomial)
    #         prob <- model %>% predict(test[, -ncol(df_encoded)], type = "response")
    #         pred <- ifelse(prob > 0.5, 1, 0)
    #         acc_valid[b] <- mean(pred==testClass)
    #     }
    #
    #     p <- plot_ly(y = acc_valid, type = 'box')
    #
    #     return(p)
    #     # boxplot(acc_valid,main="Accuracy lors des 10-fold cross validation")
    # })

    # Linear regression RMSE plot
    output$boxplotRMSE <- renderPlotly({
        B <- 10
        rmse_valid <- rep(NA,10)
        
        for (b in 1:B)
        {
            smp_size <- floor(0.75 * nrow(df_encoded))
            tr <- sample(seq_len(nrow(df_encoded)), smp_size)
            
            train <- df_encoded[tr,]
            test <- df_encoded[-tr,]
            
            model <- lm(G3~., data=train)
            pred <- predict(model, newdata = test)
            rmse_valid[b] <- sqrt(sum((exp(pred) - test$G3)^2)/length(test$G3))
        }
        
        fig <- plot_ly(y = rmse_valid, type = 'box', name='rmse on grade prediction')
        fig <- fig %>% layout(title = "Linear regression validation RMSE (10-fold CV)")

        return(fig)
    })

    # Linear regression method description.
    output$regr_info <- renderPrint({
        "In this section, we will try to predict a student grade using linear regression using not only quantitative variables but also encoded qualitative variables. Here is the RMSE result in the 10 fold cross validation!"
    })

    # Plot of difference between predicted and real grades.
    output$barplot_diff <- renderPlotly({
        set.seed(1)
        row.number <- sample(seq_len(nrow(df)), 0.65*nrow(df))

        train <- df_encoded[row.number,]
        test <- df_encoded[-row.number,]

        model <- lm(G3~., data = train)
        pred <- predict(model, newdata = test)
        # diff <- pred - test$G3

        # fig <- plot_ly(x = 1:11, y = diff[1:11], type = 'bar', name = 'student index')
        # fig <- fig %>% layout(title = '(PREDICTION - REAL GRADE) of 10 students',
        #                       xaxis = list(title = 'Student ID'),
        #                       yaxis = list(title = 'prediction - real_grade'))

        fig <- plot_ly(
            x = seq_len(nrow(test)),
            y = test$G3,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Real grade'
        )

        fig <- fig %>% add_trace(
            x = seq_along(pred),
            y = pred,
            type = 'scatter',
            mode = 'lines+markers',
            name = 'Predicted grade'
        )

        return (fig)
    })

    # KNN Classification method description.
    output$classif_info <- renderPrint({
        "We have discretized the marks column into two modalities: (succeeded, failed). The idea behind this is to predict either a student will fail or succeed in Mathematics and portuguese. To do so, we will use K nearest neighbors as a mere example"
    })

    # Unsupervised (clustering) plot.
    output$clustering_plot <- renderPlot({
        
        km.res <- kmeans(df_encoded[, -ncol(df_encoded)], input$nb_clusters, nstart = 25)
        
        fviz_cluster(km.res, df_encoded[, -ncol(df_encoded)], ellipse.type = "norm")
    })
}