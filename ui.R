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
                                         column(6, plotlyOutput(outputId = "histogrammeMod"))
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
                                    column(6, plotlyOutput(outputId = "piechartYes")),
                                    column(6, plotlyOutput(outputId = "mboxplots"))
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
                                    column(12, plotlyOutput(outputId = "gradesCorrP"))
                                ),
                                fluidRow(
                                    column(4, offset = 3, textOutput("gradesCorr")),
                                    column(8, plotlyOutput(outputId = "boxplots"))
                                )
                            )
                        )
                    ),
                    tabPanel(
                        "Analyse according to tenure",
                        fluidRow(
                            column(12, plotlyOutput(outputId = "yearsNum")),
                        ),
                        fluidRow(
                            column(12, plotlyOutput(outputId = "yearsInc"))
                        )
                    )
                )
            ),
            tabPanel(
                "Apprentissage supervisé",
                h4("Travail realisé par Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
                tabsetPanel(
                    tabPanel(
                        "KNNs",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(
                                    'k',
                                    'Select the Number K of Nearest Neighbours',
                                    value = 6,
                                    min = 1,
                                    max = 100
                                )            
                            ),
                            mainPanel(
                                fluidRow(
                                    column(6, plotlyOutput(outputId = "boxplotAcc"))
                                ),
                                column(6, plotOutput(outputId = "ROC"))
                            )
                        )
                    ),
                    tabPanel(
                        "Logistic regression",
                        fluidRow(
                            column(12, plotlyOutput(outputId = "boxplotAcc2"))
                        )
                    )
                ),
            ),
            tabPanel(
                'Unsupervised Learning',
                tabsetPanel(
                    tabPanel(
                        'KMeans Clustering + PCA (2-dim)',
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