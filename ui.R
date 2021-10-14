source('bootstrap.R')

deadpoolUI <- shinyUI({
    fluidPage(
        theme = bs_theme(bootswatch = "flatly", base_font = font_google("PT Serif", local = TRUE)),
        navbarPage(
            "Student Grade Prediction",
            tabPanel(
                "Descriptive analysis",
                h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                tabsetPanel(
                    tabPanel(
                        "Data summary",
                        fluidRow(
                            column(6, h3("Student grade prediction dataset"), textOutput(outputId = "datasetSummary")),
                            column(6, tableOutput(outputId = "dictionary"))
                        )
                    ),
                    tabPanel(
                        "Univariate analysis",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(
                                    "univariateSelect",
                                    label = h4("Select a feature for univariate analysis"),
                                    choices = sort(names(df)),
                                    selected=1
                                )
                            ),
                            mainPanel(
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "occurencesPlot")),
                                    column(6, plotlyOutput(outputId = "frequencyPlot"))
                                ),
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "cumulativeOccurencesPlot")),
                                    column(6, tableOutput(outputId = "tabStat"))
                                )
                            )
                        )
                    ),
                    tabPanel("Bivariate analysis",
                             sidebarLayout(
                                 sidebarPanel(
                                     h4("Select 2 features for bivariate analysis"),
                                     selectInput(
                                         "bivariateFirstFeature",
                                         label = "First feature",
                                         choices = names(df),
                                         selected=1
                                     ),
                                     selectInput(
                                         "bivariateSecondFeature",
                                         label="Second feature",
                                         choices = sort(names(df)),
                                         selected = 1
                                     )
                                 ),
                                 mainPanel(
                                     fluidRow(
                                         column(
                                             6,
                                             fluidRow(
                                                 column(12, plotlyOutput(outputId = "bivariateCloudPoints")),
                                                 column(4, offset = 3, textOutput("correlation"))
                                             )
                                         ),
                                         column(6, plotlyOutput(outputId = "bivariateBoxplot"))
                                     )
                                 )
                             )
                    ),
                    tabPanel("Table", dataTableOutput("table"), style = "font-size: 85%")
                )
            ),
            tabPanel(
                "Variables impact on final Grade",
                h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                tabsetPanel(
                    tabPanel(
                        "Qualitative variables",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(
                                    "selectqual",
                                    label = h4("Select a qualitative variable"),
                                    choices = categorical,
                                    selected=1
                                )
                            ),
                            mainPanel(
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "qualitativeHistogramG3")),
                                    column(6, plotlyOutput(outputId = "qualitativeBoxplotsG3"))
                                )
                            )
                        )
                    ),
                    tabPanel(
                        "Quantitave variables",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(
                                    "selectquant",
                                    label = h4("Select a quantitative variable"),
                                    choices = setdiff(quantitative, 'G3'),
                                    selected=1
                                )
                            ),
                            mainPanel(
                                fluidRow(
                                    column(12, plotlyOutput(outputId = "quantitativeCloudPointsG3"))
                                ),
                                fluidRow(
                                    column(4, offset = 3, textOutput("quantitativeCorrG3")),
                                    column(8, plotlyOutput(outputId = "quantitativeBoxplotsG3"))
                                )
                            )
                        )
                    ),
                    tabPanel(
                        "Analysis according to absence",
                        fluidRow(
                            column(12, plotlyOutput(outputId = "absenceHistogram")),
                        ),
                        fluidRow(
                            column(12, plotlyOutput(outputId = "absenceBoxplot"))
                        )
                    )
                )
            ),
            tabPanel(
                "Supervised Learning",
                h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                tabsetPanel(
                    tabPanel(
                        "Classification: succeeded or failed",
                        sidebarLayout(
                            sidebarPanel(
                                h3('K Nearest neighbors'),
                                textOutput(outputId = 'classif_info'),
                                br(),
                                br(),
                                sliderInput(
                                    'k',
                                    strong('Select the Number K of Nearest Neighbours'),
                                    value = 6,
                                    min = 1,
                                    max = 100
                                )            
                            ),
                            mainPanel(
                                fluidRow(
                                    column(6, plotOutput(outputId = "ROC")),
                                    column(6, plotlyOutput(outputId = "accuracyBoxplot"))
                                )
                                #,fluidRow(
                                #    column(6, plotlyOutput(outputId = "boxplotAcc2")),
                                
                                #)
                            )
                        )
                    ),
                    tabPanel(
                        "Multi-linear regression: Grade prediction",
                        sidebarLayout(
                            sidebarPanel(
                                h3('Linear regression'),
                                textOutput(outputId = 'regr_info')
                            ),
                            mainPanel(
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "boxplotRMSE")),
                                    column(6, plotlyOutput(outputId = "barplot_diff"))
                                )
                                #,fluidRow(
                                #    column(6, plotlyOutput(outputId = "boxplotAcc2")),
                                
                                #)
                            )
                        )
                    )
                )
            ),
            tabPanel(
                'Unsupervised Learning',
                tabsetPanel(
                    tabPanel(
                        'PCA (2-dim) + KMeans Clustering',
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(
                                    'nb_clusters',
                                    'Number of clusters',
                                    value = 3,
                                    min = 2,
                                    max = 7
                                )
                            ),
                            mainPanel(
                                fluidRow(
                                    column(12, plotOutput(outputId = "clustering_plot"))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
})