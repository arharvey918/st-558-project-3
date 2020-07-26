#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Packages to load
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
    # Header
    dashboardHeader(title = "NFL Data Analyzer"),

    # Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("View Data", tabName = "view-data", icon = icon("table")),
            menuItem("Explore Data", tabName = "explore-data", icon = icon("chart-area")),
            menuItem("Unsupervised Learning", tabName = "unsupervised-learning", icon = icon("chalkboard")),
            menuItem("Build Models", tabName = "supervised-learning", icon = icon("chalkboard-teacher"))
        )
    ),

    ### Main body content ###
    dashboardBody(
        tabItems(
            ### Information page content ###
            tabItem(
                tabName = "home",
                h2("Welcome!"),
                p("This R Shiny application can be used to perform basic, historical NFL data analysis."),
                p("There are four main sections:"),
                tags$ul(
                    tags$li(span(tags$strong("View Data"), " contains a paginated data table")),
                    tags$li(span(tags$strong("Explore Data"), " has interactive plots that explore different aspects of the data")),
                    tags$li(span(tags$strong("Unsupervised Learning"), " lets you run and visualize principal components analysis")),
                    tags$li(span(tags$strong("Build Models"), " contains interactive supervised learning tools")),
                ),
                h3("Data"),
                p("The source data used by this application is the ", span("scoresFull.csv"), " data set provided in ST 558. It contains data for NFL games from 2002 to 2014. This modified version only includes regular season games, and I also added some calculated variables to the end of the data set that are used in data exploration and model building. Additional data can be found at ", a("https://www.pro-football-reference.com/", href="https://www.pro-football-reference.com/"), "."),
                h3("About"),
                p("This R Shiny application was created by Avy Harvey in July 2020 for Project 3 of ST 558 at NC State.")
            ),

            ### Data viewer content ###
            tabItem(
                tabName = "view-data",
                fluidRow(
                    box(width = 4,
                        h3("Filters"),

                        # Needed separator so years don't show up weird: https://stackoverflow.com/a/28223518
                         sliderInput("viewDataSeasonRange", label = "Season Range", min = 2002,
                                     max = 2014, value = c(2002, 2014), sep = "")
                       )
                ),

                # Data table
                fluidRow(
                    column(12,
                           h3(textOutput("viewDataHeader")),
                           dataTableOutput('table')
                    )
                ),

                # Download button
                fluidRow(
                    column(1,
                        # Reference: https://shiny.rstudio.com/articles/download.html
                        downloadButton("downloadData", "Download Data", style="margin-top: 1rem;")
                    )
                )
            ),

            ### Data explorer content ###
            tabItem(
                tabName = "explore-data",

                h2("Explore Data"),
                p("On this page, you can interact with several pre-created plots or do your own basic analyses in the Custom Analysis section at the bottom."),
                p("Due to the volume of graphical content, this page may take several seconds to load."),

                h3("Betting Lines"),
                fluidRow(
                    # Vegas Favorite Win % by Season
                    box(
                        width = 5,
                        plotlyOutput("vegasPickPctPlot"),
                        downloadButton("downloadVegasPickPctData", "Download Plot Data")
                    ),

                    # Distribution of Vegas Spread Error
                    box(
                        width = 5,
                        checkboxInput("spreadErrorPlotEnableSeasonFilter", "Filter by Season"),
                        conditionalPanel(
                            condition = "input.spreadErrorPlotEnableSeasonFilter",
                            selectizeInput("spreadErrorPlotSeason", "Season", choices = c("Loading options...")),
                        ),
                        plotlyOutput("spreadErrorPlot"),
                        downloadButton("downloadSpreadErrorData", "Download Plot Data"),

                        # Options for customizing plot
                        span(checkboxInput("spreadErrorPlotShowNormal", "Show normal curve"), style = "display: inline-block; margin-left: 1rem;"),
                        span(checkboxInput("spreadErrorPlotShowMean", "Show mean"), style = "display: inline-block; margin-left: 1rem;")
                    ),

                    # Spread RMSE by season
                    box(
                        width = 2,
                        h4("Spread RMSE by Season"),

                        # Formula for RMSE
                        withMathJax(helpText("RMSE is calculated as: $$\\sqrt{\\sum_{i=1}^{n}{\\cfrac{(\\text{spread} - \\text{actual})^2}{n}}}$$")),
                        tableOutput("spreadRmseTable"),
                        downloadButton("downloadSpreadRmseData", "Download Table Data")
                    )
                ),

                h3("Time of Possession and Tempo"),
                fluidRow(
                    # Score differential vs. TOP
                    box(
                        width = 4,
                        plotlyOutput("scoreDiffVsTopPlot"),
                        downloadButton("downloadScoreDiffVsTopData", "Download Plot Data")
                    ),

                    # Score differential vs. total plays
                    box(
                        width = 4,
                        radioButtons("scoreDiffVsTopPlotHomeAway", "Team", selected = "Home", choices = c("Home", "Away"), inline = TRUE),
                        plotlyOutput("scoreDiffVsPlaysPlot"),
                        downloadButton("downloadScoreDiffVsPlaysData", "Download Plot Data")
                    ),

                    # Score differential vs. tempo
                    box(
                        width = 4,
                        radioButtons("scoreDiffVsTempoPlotHomeAway", "Team", selected = "Home", choices = c("Home", "Away"), inline = TRUE),
                        plotlyOutput("scoreDiffVsTempoPlot"),
                        downloadButton("downloadScoreDiffVsTempoData", "Download Plot Data")
                    )
                ),

                h3("Rushing and Passing Yards"),
                fluidRow(
                    # Average Yards by Season
                    box(
                        width = 4,
                        plotlyOutput("avgYardsPlot"),
                        downloadButton("downloadAvgYardsData", "Download Plot Data")
                    ),

                    # Yards per Carry
                    box(
                        width = 4,
                        radioButtons("ypcPlotHomeAway", "Team", selected = "Home", choices = c("Home", "Away"), inline = TRUE),
                        plotlyOutput("ypcPlot"),
                        downloadButton("downloadYpcData", "Download Plot Data")
                    ),

                    # Yards per Passing Attempt
                    box(
                        width = 4,
                        radioButtons("ypaPlotHomeAway", "Team", selected = "Home", choices = c("Home", "Away"), inline = TRUE),
                        plotlyOutput("ypaPlot"),
                        downloadButton("downloadYpaData", "Download Plot Data")
                    ),
                ),

                h3("Custom Analysis"),
                fluidRow(
                    # 5-number summary
                    box(
                        width = 4,
                        h4("Variable Summary"),
                        selectizeInput("customSummaryTableVar", "Variable", choices = c("Loading options...")),
                        tableOutput("customSummaryTable"),
                        downloadButton("downloadCustomSummaryData", "Download Table Data")
                    ),

                    # Scatterplot
                    box(
                        width = 8,
                        h4("Scatterplot"),
                        selectizeInput("customPlotVar1", "Y Variable", choices = c("Loading options...")),
                        selectizeInput("customPlotVar2", "X Variable", choices = c("Loading options...")),
                        plotlyOutput("customPlot"),
                        downloadButton("downloadCustomData", "Download Plot Data")
                    )
                )

            ),

            ### Unsupervised learning content ###
            tabItem(
                tabName = "unsupervised-learning",
                h2("Principal Components Analysis"),
                p("Create a biplot of the principal components by selecting at least two numeric variables."),
                fluidRow(
                    box(
                        width = 12,
                        selectizeInput("pcaVars", "Variables", choices = c("Loading options..."), multiple = TRUE),
                        checkboxInput("pcaVarsScale", "Scale variables to have unit variance", value = TRUE)
                    )
                ),
                fluidRow(
                    box(
                        h3("Biplot"),
                        conditionalPanel("input.pcaVars.length < 2",
                                         p("Select at least two variables to see the biplot.")
                        ),
                        conditionalPanel("input.pcaVars.length >= 2",
                                         plotOutput("pcaPlot"),
                                         downloadButton("downloadPcaPlotData", "Download Plot Data")
                        )
                    ),

                    box(
                        h3("Variable Loadings"),
                        conditionalPanel("input.pcaVars.length < 2",
                                         p("Select at least two variables to see the PCA variable loadings.")
                        ),
                        conditionalPanel("input.pcaVars.length >= 2",
                                         dataTableOutput("pcaRotationTable"),
                                         downloadButton("downloadPcaRotationData", "Download Table Data")
                        )
                    )
                )
            ),

            ### Supervised learning content ###
            tabItem(
                tabName = "supervised-learning",
                h2("Build Models"),
                p("On this page, you can build machine learning models to make predictions about whether a team will win or not based on certain predictor variables. Note that any variables in the data that are outcome-related, such as `homeWin`, `HminusAScore`, `spreadDiff`, and others are not included in the available variables list."),

                # Classification tree
                h2("Classification Tree"),
                fluidRow(
                    box(
                        h3("Training Parameters"),
                        actionButton("treeUseSuggestedVarsBtn", "Use Suggested Variables"),
                        actionButton("treeUseAllVarsBtn", "Use All Variables"),
                        selectizeInput("treeVars", "Variables", choices = c("Loading options..."), multiple = TRUE),
                        numericInput("treeCp", "Complexity parameter for pruning",
                                     min = 0, value = 0.02),
                        sliderInput("treeCvFolds", "Number of folds to use in cross validation",
                                    min = 3, max = 10, value = 3),
                        actionButton("treeTrainButton", "Train Model", icon = icon("cogs"), class = "btn-primary", style = "color: #fff;")
                    ),

                    box(
                        h3("Results"),
                        conditionalPanel(
                            "!output.treeModelCreated",
                            p("Train a model to see results.")
                        ),
                        conditionalPanel(
                            "output.treeModelCreated",
                            h4("Parameters"),
                            p("Variables used: ", uiOutput("treeVarsText")),
                            p(span("Number of variables randomly sampled at each split: ", textOutput("treeMtryText", inline = TRUE))),
                            p(span("Number of cross validation folds: ", textOutput("treeCvFoldsText", inline = TRUE))),

                            h4("Accuracy"),
                            p("Cross-validation accuracy: ", textOutput("treeCvAccuracyText", inline = TRUE)),
                            p("Holdout test data accuracy: ", textOutput("treeTestAccuracyText", inline = TRUE)),
                        ),
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        h3("Predict New Data"),
                        p("You can make predictions in this area after a model has been trained."),
                        conditionalPanel(
                            "output.treeModelCreated",
                            uiOutput("treePredictionForm"),
                            actionButton("treePredictButton", "Make Prediction", icon = icon("magic"), class = "btn-primary", style = "color: #fff;"),
                            conditionalPanel(
                                "output.treePredictionMade",
                                tags$hr(),
                                h4("Prediction Result"),
                                p("Predictors were:"),
                                # Scroll-x: https://github.com/rstudio/shinydashboard/issues/40
                                div(style = 'overflow-x: auto', tableOutput("treePredictionInputTable")),
                                p("Prediction: ", strong(textOutput("treePredictionOutput", inline = TRUE)))
                            )
                        )
                    )
                ),

                # Random Forest
                h2("Random Forest"),
                fluidRow(
                    box(
                        h3("Training Parameters"),
                        actionButton("randomForestUseSuggestedVarsBtn", "Use Suggested Variables"),
                        actionButton("randomForestUseAllVarsBtn", "Use All Variables"),
                        selectizeInput("randomForestVars", "Variables", choices = c("Loading options..."), multiple = TRUE),
                        sliderInput("randomForestMtry", "Number of variables to randomly sample at each split",
                                    min = 1, max = 10, value = 3),
                        sliderInput("randomForestCvFolds", "Number of folds to use in cross validation",
                                    min = 3, max = 10, value = 3),
                        actionButton("randomForestTrainButton", "Train Model", icon = icon("cogs"), class = "btn-primary", style = "color: #fff;")
                    ),

                    box(
                        h3("Results"),
                        conditionalPanel(
                            "!output.randomForestModelCreated",
                            p("Train a model to see results.")
                        ),
                        conditionalPanel(
                            "output.randomForestModelCreated",
                            h4("Parameters"),
                            p("Variables used: ", uiOutput("randomForestVarsText")),
                            p(span("Number of variables randomly sampled at each split: ", textOutput("randomForestMtryText", inline = TRUE))),
                            p(span("Number of cross validation folds: ", textOutput("randomForestCvFoldsText", inline = TRUE))),

                            h4("Accuracy"),
                            p("Cross-validation accuracy: ", textOutput("randomForestCvAccuracyText", inline = TRUE)),
                            p("Holdout test data accuracy: ", textOutput("randomForestTestAccuracyText", inline = TRUE)),
                        ),
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        h3("Predict New Data"),
                        p("You can make predictions in this area after a model has been trained."),
                        conditionalPanel(
                            "output.randomForestModelCreated",
                            uiOutput("randomForestPredictionForm"),
                            actionButton("randomForestPredictButton", "Make Prediction", icon = icon("magic"), class = "btn-primary", style = "color: #fff;"),
                            conditionalPanel(
                                "output.randomForestPredictionMade",
                                tags$hr(),
                                h4("Prediction Result"),
                                p("Predictors were:"),
                                # Scroll-x: https://github.com/rstudio/shinydashboard/issues/40
                                div(style = 'overflow-x: auto', tableOutput("randomForestPredictionInputTable")),
                                p("Prediction: ", strong(textOutput("randomForestPredictionOutput", inline = TRUE)))
                            )
                        )
                    )
                )
            )
        )
    )
)
