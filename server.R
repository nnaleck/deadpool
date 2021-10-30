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
    
    # Table display
    output$table <- renderDataTable({df})
    
    # Reactive variable giving frequency measures on modalities of qualitative variables
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
    
    # Dataset dictionary import from excel file
    output$dictionary <- renderTable({
        dict
    })
    
    # Data summary
    output$datasetSummary <- renderText({
        "<p style=\"font-size:120%\">
        <br/>
        This data approach student achievement in secondary education of two Portuguese schools. It was actually collected using school reports and questionnaires.<br/><br/>
        The data attributes include student grades, demographic, social and school-related features. The target attribute is the final grade of mathematics which is represented by G3 <br/><br/>
        <b>Important note:</b><br/>
        <i>The target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful (see paper source for more details).</i>
        </p>"
    })
    
    # Statistic measures based on variable type
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
    
    # Univariate conclusions
    output$uniConclusions <- renderText({
        "<h4><b><center>Students univariate analysis</h4></b></center>  <br/><br/>
        We have got in our hands a set of students with quite diverse profiles. We will start by analysing theses profiles based on each attribute independently. 
        <i><b>Let's get to know the students through simple questions!</b></i>  <br/><br/>
        <b>What is the age of the students?</b>  <br/>
        Actually, the students age vary from 15 to 22 but only 1% of the students are 20 years old or more and only 6% are 19 years old. Moreover, all the ages from 15 to 18 represent 21% each.
        Which means that we may have got 3 or 4 consecutif and different schools levels.  <br/><br/>
        <b>Are the students living in a rural or urban area?</b>  <br/>
        Well both! Out of a total of 395 students, 307(77.7%) of the students live in an urban area. However 88(22.3%) students live in a rural area. <br/><br/>
        <b>Do they practice any extracurricular activities?</b>  <br/>
        Half of them does (50.9%), half doesn't (49.1%). <br/><br/>
        <b>Have the students failed a class throughout their education?</b>  <br/>
        Fortunately, 79% of the students have never failed a class. Nevertheless, 12% of the studens have failed a single class in the past and 4%(16) of the students have failed 3 years already.<br/><br/>
        <b>What about their fathers, is he educated?</b>  <br/>
        Yes, indeed for the most part. 2 student's fathers aren't educated. However, only 24.3% have higher degrees. All the remaining fathers have a secondary education or less.<br/><br/>
        <b>Last but not least, how are their grades?</b>  <br/>
        We got 4 groups of students (with the same propotions: 25% each) in term of their grades: <br/>
        <ol>
        <li>Grade between 0 and 8</li>
        <li>Grade between 9 and 11</li>
        <li>Grade between 12 and 14</li>
        <li>Grade between 15 and 20</li>
        </ol>
        "
        
    })
    
    # Bivariate conclusions
    output$biConclusions <- renderText({
        "<h4><b><center>Students bivariate analysis</h4></b></center>  <br/><br/>
        Now that we got to know the students through a simple univariate analysis, it would be interesting to know the students in rather a bidimensional space where we can unveal attributes correlations. 
        <i><b>To do so, we will stick to our question answering strategy!</b></i>  <br/><br/>
        <b>Does the students have more absences as they get older?</b>  <br/>
        Your intuition is correct! Actually, all the student who are 15 years old have less than 14 absences while the older students have all kind of number of absences that can reach 75 absences. Well it's actually a single student but still, they have a got a way higher mean of absences comparing to the younger ones!<br/><br/>
        <b>Is having failed several times potentially caused by being frequently absent?</b>  <br/>
        Somehow. More than half ot the student that have failed many time have 0 absences while the other half have less than 20 absences. Moreover, 37% of these students have more than 7 absences. So yes, it isn't a direct cause for this specific sample.<br/><br/>
        <b>How about alcohol consumption? Is it in any way related to failing?</b>  <br/>
        Many would think that alcohol abuse may be causing some students to fail. Well it is in general true, in this case, it isn't the case. Almost all of the students that consume alcohol heavily (Dalc: 3 to 5 in scale of 1 to 5), have 0 failures. This proves that these students know how to handle party/work trade-off.<br/><br/>
        "
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
        df_tmp$absences <- tenure
        df_tmp = df_tmp[order(df_tmp$absences),]
        p <- ggplot(
            df_tmp,
            aes_string(x = 'G3' , fill='absences')
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
        df_tmp$absences <- tenure
        df_tmp = df_tmp[order(df_tmp$absences),]
        
        p <- qplot(
            x = df_tmp$absences,
            y = df_tmp$G3,
            xlab = "Modalités",
            ylab = "Final grade(G3)",
            geom = "boxplot",
            fill=df_tmp$Tenure
        ) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
        return(ggplotly(p))
    })
    
    # Interpretation of impact of variables on final grade
    output$gradeConclusions <- renderText({
        "<h5><b>How does the different attributes affect the students final grade?</b></h5><br/><br/>
        In this section, we are searching for any form of correlation or, if we are lucky, some form of causation to the final grade.<br/>
        To get there, we applied a bivariate analysis while fixing the final grade as one of the parameters.<br/><br/>
        <i><b>We will restrict our analyis on the following traits:</b></i>
        <ul>
        <li>Adress (rural or urban area)</li>
        <li>Alcohol consumption</li>
        <li>Absences</li>
        <li>study time</li>
        </ul>
        <br/>
        <b>1. Area type:</b><br/>
        First of all, the final grade of students living in a rural area has almost the same distribution as the final grade of those living in an urban area.<br/>
        To illustrate, 4.5% of rural students have a grade higher than 18. Same as urban students with a percentage of 6.5%. We believe that the adresse isn't correlated to final grade!<br/><br/>
        <b>2. Alcohol consumption:</b><br/>
        Secondly, alcohol consumption seem to be a very good discriminator of excellent students. Actually, all those who consume alcohol heavily have at most 13 in their final grade.
        <br/>Those who consume moderately can have good marks but never got more than 18. Only those who don't drink that had exellent grades. <br/> 
        This is due to the fact that these student are still young. They are in the begining of the adulthood journey so they may lose control over priorities especially with parties and alcohol.<br/><br/> 
        <b>3. Absences</b><br/>
        The first thing that we noticed looking at the grade/absences scatter plot is that the students that have many absences are average students. <br/>
        We can't extract any more information since there is evidently no correlation between these variables. Apparently the students study at home to fill for their absences.<br/><br/> 
        <b>4. Study time:</b><br/>
        Hard work doesn't alway pays off! Among those who study at home the most (more than 10 hours per week), you will find almost every grade. <br/>
        Not only that but we observed that the variance is maximal. The grade can go from 0 to 20 with almost a uniform distribution. This means that working hard alone won't add up to anything.<br/> 
        You will need most of all passion, a good learning strategy and solid basics!"
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
    output$regr_info <- renderText({
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
    output$classif_info <- renderText({
        "We have discretized the marks column into two modalities: (succeeded, failed). The idea behind this is to predict either a student will fail or succeed in Mathematics and portuguese. To do so, we will use K nearest neighbors as a mere example"
    })
    
    # KNN Classification results interpretation
    output$knnInterpretation <- renderText({
        "Our validation accuracy varies from 0.6 to 0.75 which is not bad at all since we didn't use the algorithm properly. This is because all the variable are qualitative and instead of using the proper distance such as khi2, we onehot encoded the variables instead. The impact of this forced transformation we applied on our data to convert it in a way (SHOULD'T BE DONE) into quantitative variables can be seen in the ROC CURVE. The AUC surface isn't good."
    }) 
    
    # Linear regression
    output$regrConclusions <- renderText({
        "The linear regression didn't give good results. This is mainly due to the fact that this method is supposed to be applied when the data is quantitative which isn't the case here. Same as with knn, we encoded the variables. The model managed sometime to predict the grade with a very small marge."
    })
    # Unsupervised (clustering) plot.
    output$clustering_plot <- renderPlot({
        
        km.res <- kmeans(df_encoded[, -ncol(df_encoded)], input$nb_clusters, nstart = 25)
        
        fviz_cluster(km.res, df_encoded[, -ncol(df_encoded)], ellipse.type = "norm")
    })
    
    
 
}