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
                    column(12,
                           dataTableOutput('table')
                    )
                ),
                fluidRow(
                    column(1,
                        # Reference: https://shiny.rstudio.com/articles/download.html
                        downloadButton("downloadData", "Download All Data", style="margin-top: 1rem;")
                    )
                )
            ),
            
            ### Data explorer content ###
            tabItem(
                tabName = "explore-data",
                h2("Explore data content"),
                fluidRow(
                    box(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30)
                    ),
                    box(
                        plotOutput("distPlot")
                    )
                ),
                fluidRow(
                    box(
                        plotlyOutput("vegasPickPct")
                    )
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
