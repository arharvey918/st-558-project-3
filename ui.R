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
                p("This is some information about this application.")
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
