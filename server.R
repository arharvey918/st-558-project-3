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
        mutate(day = as.factor(day),
               season_num = season) %>%
        mutate(season = as.factor(season)) %>%
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

# Function to create a CSV download handler for a data frame
get_download_handler <- function(df, basename = "data") {
    downloadHandler(
        filename = paste0(basename, ".csv"),
        content = function(file) {
            write.csv(df, file, row.names = TRUE)
        }
    )
}

# Get the data we'll use for this app
scores_df <- get_data()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #################################
    # DATA VIEWER OUTPUTS
    #################################
    
    # Season filter
    updateSliderInput(session, "viewDataSeasonRange",
                         min = min(as.numeric(levels(scores_df$season))),
                         max = max(as.numeric(levels(scores_df$season))),
                         value = c(min(as.numeric(levels(scores_df$season))),
                                   max(as.numeric(levels(scores_df$season)))))
    
    view_data_df <- reactive({
        scores_df %>%
            filter(season_num >= input$viewDataSeasonRange[1] & season_num <= input$viewDataSeasonRange[2])
    })
    
    output$viewDataHeader <- renderText({
        paste("Data from", input$viewDataSeasonRange[1], "to", input$viewDataSeasonRange[2], sep = " ")
    })
        
    # Data table
    # Reference: https://shiny.rstudio.com/reference/shiny/1.4.0/tableOutput.html
    # Reference: https://stackoverflow.com/a/30765558
    output$table <- renderDataTable(view_data_df(),
                                    options = list(scrollX = TRUE))
    
    # Downloadable csv of scores data
    # Have to do this explicitly for reactivity
    # Reference: https://shiny.rstudio.com/articles/download.html
    output$downloadData <- downloadHandler(
        filename = "scores.csv",
        content = function(file) {
            write.csv(view_data_df(), file, row.names = TRUE)
        }
    )
    
    
    #################################
    # EXPLORE DATA OUTPUTS
    #################################
    
    output$vegasPickPctPlot <- renderPlotly({
        # Summarize data
        df <- scores_df %>%
            filter(homeSpread != 0) %>%
            select(season, isFavoriteTheWinner) %>%
            group_by(season) %>%
            summarize(vegasCorrectPct = sum(isFavoriteTheWinner) / n())
        
        # Handler for data download
        output$downloadVegasPickPctData <- get_download_handler(df)
        
        # Create plot
        p <- ggplot(df, aes(x = season, y = vegasCorrectPct)) + 
            geom_bar(stat = "identity") +
            geom_hline(aes(yintercept = mean(vegasCorrectPct)), color = "blue", linetype="dashed") +
            coord_cartesian(ylim = c(0.5, 0.8)) +
            labs(title = "Vegas Favorite Win % by Season", x = "Season", y = "Vegas Favorite Win %")
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
    
    output$spreadErrorPlot <- renderPlotly({
        # Dynamic title and filtered data
        title <- "Distribution of Vegas Spread Error"
        df <- scores_df
        if (input$spreadErrorPlotEnableSeasonFilter) {
            title <- paste0(title, " in ", input$spreadErrorPlotSeason)
            df <- scores_df %>%
                filter(season == input$spreadErrorPlotSeason)
        }
        
        # Handler for data download
        output$downloadSpreadErrorData <- get_download_handler(df)
        
        # Histogram of predicted spread - actual
        p <- ggplot(df, aes(x = spread_diff)) +
            geom_histogram(binwidth = 3, aes(y = stat(density)), alpha = 0.5) +
            geom_density(kernel = "gaussian", fill = "blue", alpha = 0.5, color = "blue") +
            labs(title = title, x = "Vegas Spread - Actual Spread", y = "Density")
        
        # Conditionally overlay the normal curve
        if (input$spreadErrorPlotShowNormal) {
            p <- p +
                # Referenced code: https://stackoverflow.com/a/13609916
                stat_function(fun = dnorm, args = list(mean = mean(scores_df$spread_diff), sd = sd(scores_df$spread_diff)), color = "orange")
        }
        
        # Conditionally overly the mean as a reference line
        if (input$spreadErrorPlotShowMean) {
            p <- p + geom_vline(aes(xintercept = mean(spread_diff)), linetype = "dashed")
        }
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })

    # Season filter for spread error plot    
    updateSelectizeInput(session, "spreadErrorPlotSeason",
                         choices = levels(scores_df$season))
    
    output$spreadRmseTable <- renderTable({
        # RMSE of predicted spread vs. actual
        df <- scores_df %>%
            group_by(season) %>%
            summarise(spread_rmse = sqrt(mean(spread_diff^2))) %>%
            rename(Season = season, `Spread RMSE` = spread_rmse)
        
        df
    })
    
    output$scoreDiffVsTopPlot <- renderPlotly({
        # Handler for data download
        output$downloadScoreDiffVsTopData <- get_download_handler(scores_df)
        
        # Create plot
        p <- ggplot(scores_df, aes(x = homeTOP, y = HminusAScore)) +
            geom_point(aes(color = season), alpha = 0.5) +
            geom_smooth(method = lm) +
            labs(title = "Score Differential vs. TOP", x = "TOP (Home)", y = "Score Differential (Home)")
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
    
    output$scoreDiffVsPlaysPlot <- renderPlotly({
        # Handler for data download
        output$downloadScoreDiffVsPlaysData <- get_download_handler(scores_df)
        
        # Create plot
        p <- ggplot(scores_df, aes(x = HtotalPlays, y = HminusAScore))
        if (input$scoreDiffVsTopPlotHomeAway == "Away") {
            p <- ggplot(scores_df, aes(x = AtotalPlays, y = -HminusAScore))
        }
        
        p <- p +
            geom_point() +
            geom_smooth() +
            labs(title = "Score Differential vs. Total Plays", x = "Total Plays", y = "Score Differential")
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
    
    output$scoreDiffVsTempoPlot <- renderPlotly({
        # Handler for data download
        output$downloadScoreDiffVsTempoData <- get_download_handler(scores_df)
        
        # Create plot
        p <- ggplot(scores_df, aes(x = homePlaysPerMinute, y = HminusAScore))
        if (input$scoreDiffVsTempoPlotHomeAway == "Away") {
            p <- ggplot(scores_df, aes(x = awayPlaysPerMinute, y = -HminusAScore))
        }

        p <- p +
            geom_point() +
            geom_smooth() +
            labs(title = "Score Differential vs. Plays per Minute", x = "Plays per Minute", y = "Score Differential")
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })

    output$avgYardsPlot <- renderPlotly({
        # Summarize data
        df <- scores_df %>%
            mutate(rushing = (homeRushYds + awayRushYds) / 2,
                   passing = (homePassYds + awayPassYds) / 2) %>%
            group_by(season) %>%
            summarize(rushing = mean(rushing), passing = mean(passing)) %>%
            pivot_longer(2:3, names_to = "type", values_to = "value")
        
        # Handler for data download
        output$downloadAvgYardsData <- get_download_handler(df)
        
        # Create plot
        p <- ggplot(df, aes(x = season, y = value, fill = type)) +
            geom_bar(stat = "identity") +
            labs(title = "Average Yards Gained by Season", x = "Season", y = "Average Yards Gained") +
            coord_flip()
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
    
    output$ypcPlot <- renderPlotly({
        # Handler for data download
        output$downloadYpcData <- get_download_handler(scores_df)
        
        # Create plot
        p <- ggplot(scores_df, aes(x = homeYPC, color = homeWin, fill = homeWin))
        if (input$ypcPlotHomeAway == "Away") {
            p <- ggplot(scores_df, aes(x = awayYPC, color = awayWin, fill = awayWin))
        }
        
        p <- p +
            geom_density(alpha = 0.5) +
            labs(title = "Distribution of Yards per Carry", x = "Yards Per Carry", y = "Density")
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
    
    output$ypaPlot <- renderPlotly({
        # Handler for data download
        output$downloadYpaData <- get_download_handler(scores_df)
        
        # Create plot
        p <- ggplot(scores_df, aes(x = homeYPA, color = homeWin, fill = homeWin))
        if (input$ypaPlotHomeAway == "Away") {
            p <- ggplot(scores_df, aes(x = awayYPA, color = awayWin, fill = awayWin))
        }
        
        p <- p +
            geom_density(alpha = 0.5) +
            labs(title = "Distribution of Yards per Passing Attempt", x = "Yards Per Passing Attempt", y = "Density")
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
})
