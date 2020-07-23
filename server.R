#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny)
library(tidyverse)
library(DT)
library(plotly)

# Function to prepare data and return it
get_data <- function() {
    scores <- read_csv("scoresFull.csv")
    scores_df <- scores %>%
        filter(week %in% as.character(1:17)) %>%  # Regular season only
        mutate(day = as.factor(day)) %>%
        discard(is.character) %>%
        mutate(awayWin = (AFinal > HFinal),
               homeWin = (HFinal > AFinal),
               awayYPC = awayRushYds / awayRushAtt,
               homeYPC = homeRushYds / homeRushAtt,
               awayYPA = awayPassYds / awayPassAtt,
               homeYPA = homePassYds / homePassAtt,
               awayTPP = Aturnovers / AtotalPlays,
               homeTPP = Hturnovers / HtotalPlays,
               away3rdPct = away3rdConv / away3rdAtt,
               home3rdPct = home3rdConv / home3rdAtt,
               awayPenPerPlay = awayNumPen / AtotalPlays,
               homePenPerPlay = homeNumPen / HtotalPlays,
               homePlaysPerMinute = HtotalPlays / homeTOP,
               awayPlaysPerMinute = AtotalPlays / awayTOP,
               isFavoriteTheWinner = (homeSpread > 0 & homeWin) | (homeSpread < 0 & !homeWin),
               spread_diff = homeSpread - HminusAScore)
    
    return(scores_df)
}

# Get the data we'll use for this app
scores_df <- get_data()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #################################
    # DATA VIEWER OUTPUTS
    #################################
    
    # Data table
    # Reference: https://shiny.rstudio.com/reference/shiny/1.4.0/tableOutput.html
    # Reference: https://stackoverflow.com/a/30765558
    output$table <- renderDataTable(scores_df,
                                    options = list(scrollX = TRUE))
    
    # Downloadable csv of scores data
    # Reference: https://shiny.rstudio.com/articles/download.html
    output$downloadData <- downloadHandler(
        filename = "scores.csv",
        content = function(file) {
            write.csv(scores_df, file, row.names = TRUE)
        }
    )
    
    #################################
    # EXPLORE DATA OUTPUTS
    #################################
    
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    output$vegasPickPct <- renderPlotly({
        p <- scores_df %>%
            filter(homeSpread != 0) %>%
            select(season, isFavoriteTheWinner) %>%
            group_by(season) %>%
            summarize(vegasCorrectPct = sum(isFavoriteTheWinner) / n()) %>%
            ggplot(aes(x = season, y = vegasCorrectPct)) + 
            geom_line() +
            geom_point() +
            geom_hline(aes(yintercept = mean(vegasCorrectPct)), color = "blue", linetype="dashed") +
            labs(title = "Vegas Favorite Win % by Season", x = "Season", y = "Vegas Favorite Win %")
        
        # View it with plotly
        ggplotly(p) %>%
            layout(dragmod = "pan")
    })

})
