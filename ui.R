#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                        
                        # Need separator: https://stackoverflow.com/a/28223518
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
                        span(checkboxInput("spreadErrorPlotShowNormal", "Show normal curve"), style = "display: inline-block; margin-left: 1rem;"),
                        span(checkboxInput("spreadErrorPlotShowMean", "Show mean"), style = "display: inline-block; margin-left: 1rem;")
                    ),
                    
                    # Spread RMSE by season
                    box(
                        width = 2,
                        h4("Spread RMSE by Season"),
                        withMathJax(helpText("RMSE is calculated as: $$\\sqrt{\\sum_{i=1}^{n}{\\cfrac{(\\text{spread} - \\text{actual})^2}{n}}}$$")),
                        tableOutput("spreadRmseTable")
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
                )
            ),
            
            ### Unsupervised learning content ###
            tabItem(
                tabName = "unsupervised-learning",
                h2("Unsupervised learning content")
            ),
            
            ### Supervised learning content ###
            tabItem(
                tabName = "supervised-learning",
                h2("Build models content")
            )
        )
    )
)
