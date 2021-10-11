source('univariate.R')
source('bivariate.R')

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
        if(
            ! is.numeric(df[, input$bivariateFirstFeature])
            |
            ! is.numeric(df[, input$bivariateSecondFeature])
        ) return(NULL)

        coeff_correlation.tmp <- cov(df[, input$bivariateFirstFeature], df[, input$bivariateSecondFeature])/(sqrt(var(df[, input$bivariateFirstFeature])*var(df[, input$bivariateSecondFeature])))
        paste('Coeff de corrélation linéaire = ', round(coeff_correlation.tmp, digits=2))
    })

    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(df, is.numeric))
        corrMatrix.tmp = round(cor(df[, nums.tmp]), 2)

        heatmap(corrMatrix.tmp)
    })

    output$histogrammeMod <- renderPlotly({
        if( is.numeric(df[, input$bivariateFirstFeature]) & is.numeric(df[, input$bivariateSecondFeature])){
            columns <- c(input$bivariateFirstFeature, input$bivariateSecondFeature)
            # Reshape data()
            data.stack <- melt(df[, columns], measure.vars = columns)
            # Boxplot élaborée
            p <- qplot(x = data.stack[,1], y = data.stack[,2],
                       xlab = "Modalités", ylab = "Mesures",
                       geom=c("boxplot"), fill=data.stack[,1]) +
                theme(legend.title=element_blank())

            return(ggplotly(p))
        }
        if ( is.numeric(df[, input$bivariateFirstFeature]) & !is.numeric(df[, input$bivariateSecondFeature])){

            p <- qplot(x = df[, input$bivariateSecondFeature], y = df[, input$bivariateFirstFeature],
                       xlab = "Modalités", ylab = "Mesures",
                       geom=c("boxplot"), fill=df[, input$bivariateSecondFeature]) +
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

            return(ggplotly(p))
        }
        if ( !is.numeric(df[, input$bivariateFirstFeature]) & is.numeric(df[, input$bivariateSecondFeature])){
            p <- ggplot(data=df)+
                geom_boxplot(mapping = aes_string(input$bivariateFirstFeature, input$bivariateSecondFeature))+
                xlab(label = input$bivariateFirstFeature)+
                ylab(label=input$bivariateSecondFeature)+
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

            return(ggplotly(p))
        }

    })

    output$piechartYes <- renderPlotly({
        p <- ggplot(data=df)+
            geom_histogram(mapping = aes_string('G3', fill=input$selectqual), bins = 10)+
            xlab(label = 'Final grade')+
            ylab(label="Frequency")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    })

    output$mboxplots <- renderPlotly({
        p <- qplot(x = df[, input$selectqual], y = df[, "G3"],
                   xlab = paste("Modalités de", input$selectqual, sep=" "), ylab = "Final grade",
                   geom=c("boxplot","jitter"), fill= df[, input$selectqual]) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    })


    output$yearsNum <- renderPlotly({
        df_tmp <- data.frame(df)
        tenure <- sapply(df$absences, discretize)
        df_tmp$Tenure <- tenure
        p <- ggplot(df_tmp, aes_string(x = 'G3' , fill='Tenure')) +
            geom_histogram() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
            labs(title= paste("Tenure groups count"))

        return(ggplotly(p))
    })

    output$yearsInc <- renderPlotly({
        df_tmp <- data.frame(df)
        tenure <- sapply(df$absences, discretize)
        df_tmp$Tenure <- tenure
        p <- qplot(x = df_tmp$Tenure, y = df_tmp$G3,
                   xlab = "Modalités", ylab = "Final grade(G3)",
                   geom=c("boxplot"), fill=df_tmp$Tenure) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    })

    output$gradesCorrP <- renderPlotly({
        p <- plot_ly(
            x = df[, input$selectquant],
            y = df[, 'G3'],
            name=paste('Final grade with regards to ', input$selectquant),
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

    output$gradesCorr <- renderText({
        coeff_correlation.tmp <- cov(df$G3, df[, input$selectquant])/(sqrt(var(df$G3)*var(df[, input$selectquant])))
        paste('Coeff de corrélation linéaire = ', round(coeff_correlation.tmp, digits=2))
    })

    output$boxplots <- renderPlotly({
        columns = c('G3', input$selectquant)
        # Reshape data()
        data.stack <- melt(df[, columns], measure.vars = columns)
        # Boxplot élaborée
        p <- qplot(x = data.stack[,1], y = data.stack[,2],
                   xlab = "Modalités", ylab = "Mesures",
                   geom=c("boxplot"), fill=data.stack[,1]) +
            theme(legend.title=element_blank())

        return(ggplotly(p))
    })

    output$clustering_plot <- renderPlot({
        # Keeping only continuous variables
        df <- df[, ! names(df) %in% categorical]

        km.res <- kmeans(df, input$nb_clusters, nstart = 25)

        fviz_cluster(km.res, df, ellipse.type = "norm")
    })

    output$pca_plot <- renderPlotly({
        df <- df[, ! names(df) %in% categorical]

        pca <- prcomp(df, center = TRUE, scale = TRUE)

        # Only keeping PC1 & PC2
        p <- ggbiplot(pca, labels = rownames(df))

        return(ggplotly(p))
    })

    output$boxplotAcc <- renderPlotly({
        B <- 10
        acc_valid <- rep(NA,10)

        for (b in 1:B)
        {
            smp_size <- floor(0.75 * nrow(df_encoded))
            tr <- sample(1:nrow(df_encoded),smp_size)

            train <- df_encoded[tr,]
            trainClass <- sapply(train$G3, discretizeGrades)
            test <- df_encoded[-tr,]
            testClass <- sapply(test$G3, discretizeGrades)
            ka <- input$k

            pred <- knn(train[, -ncol(df_encoded)],test[, -ncol(df_encoded)],trainClass,k=ka)
            acc_valid[b] <- mean(pred==testClass)
        }

        plot_ly(y = acc_valid, type = 'box')
        # boxplot(acc_valid,main="Accuracy lors des 10-fold cross validation")
    })

    output$ROC <- renderPlot({
        smp_size <- floor(0.75 * nrow(df_encoded))
        tr <- sample(1:nrow(df_encoded),smp_size)

        train <- df_encoded[tr,]
        trainClass <- sapply(train$G3, binarize)
        test <- df_encoded[-tr,]
        testClass <- sapply(test$G3, binarize)

        prob <- rep(NA, nrow(test))
        ka <- input$k
        res <- knn(train[, -ncol(df_encoded)],test[, -ncol(df_encoded)],trainClass,k=ka, prob=TRUE)
        prob[res==1] <- attr(res,"prob")[res==1]
        prob[res==0] <- 1-attr(res,"prob")[res==0]

        pred <- prediction(prob, testClass)
        perf <- performance(pred, "tpr", "fpr")

        plot(perf, main="Courbe ROC") #courbe ROC
        abline(a=0, b=1)
    })

    output$boxplotAcc2 <- renderPlotly({
        B <- 10
        acc_valid <- rep(NA,10)

        for (b in 1:10)
        {
            smp_size <- floor(0.75 * nrow(df))
            tr <- sample(1:nrow(df),smp_size)


            train <- df_encoded[tr,]
            trainClass = sapply(train$G3, binarize)
            test <- df_encoded[-tr,]
            testClass = sapply(test$G3, binarize)
            train = train[, -ncol(df_encoded)]
            train['G3'] = as.numeric(trainClass)
            # Fit the model
            model <- glm(  G3 ~., data = train, family = binomial)
            prob <- model %>% predict(test[, -ncol(df_encoded)], type = "response")
            pred <- ifelse(prob > 0.5, 1, 0)
            acc_valid[b] <- mean(pred==testClass)
        }

        p <- plot_ly(y = acc_valid, type = 'box')

        return(p)
        # boxplot(acc_valid,main="Accuracy lors des 10-fold cross validation")
    })
}