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
library(caret)

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

        # Handler for data download
        output$downloadSpreadRmseData <- get_download_handler(df)
        
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
    
    output$customSummaryTable <- renderTable({
        # Code referenced from HW7 key
        df <- apply(scores_df %>% select(input$customSummaryTableVar), 2, summary)
        
        # Handler for data download
        output$downloadCustomSummaryData <- get_download_handler(df)
        
        df
    }, rownames = TRUE)
    
    # Variable 1 filter for custom plot
    updateSelectizeInput(session, "customSummaryTableVar",
                         choices = names(scores_df %>% select_if(is.numeric)))
    
    output$customPlot <- renderPlotly({
        # Handler for data download
        output$downloadCustomData <- get_download_handler(scores_df)
        
        # Create plot
        p <- ggplot(scores_df, aes_string(y = input$customPlotVar1, x = input$customPlotVar2)) +
            geom_point()
        
        # View it with plotly
        ggplotly(p) %>%
            layout()
    })
    
    # Variable 1 filter for custom plot
    updateSelectizeInput(session, "customPlotVar1",
                         choices = names(scores_df))
    
    # Variable 2 filter for custom plot
    updateSelectizeInput(session, "customPlotVar2",
                         choices = names(scores_df))


    #################################
    # UNSUPERVISED LEARNING OUTPUTS
    #################################
    
    # Variable filter for PCA
    updateSelectizeInput(session, "pcaVars",
                         choices = names(scores_df %>% select_if(is.numeric)))
    
    principal_components <- reactive({
        if (length(input$pcaVars) >= 2) {
            prcomp(select(scores_df, input$pcaVars), scale = input$pcaVarsScale)
        } else {
            NULL
        }
    })
    
    output$pcaPlot <- renderPlot({
        if (length(input$pcaVars) >= 2) {
            # Handler for data download
            output$downloadPcaPlotData <- get_download_handler(principal_components()$x)
            
            # Create plot
            biplot(principal_components(), xlabs = rep(".", nrow(scores_df)), cex = 1)
        }
    })
    
    output$pcaRotationTable <- renderDataTable({
        if (length(input$pcaVars) >= 2) {
            # Handler for data download
            output$downloadPcaRotationData <- get_download_handler(principal_components()$rotation)
            
            # Return the data
            principal_components()$rotation
        }
    }, rownames = TRUE, options = list(scrollX = TRUE))
    
    #################################
    # SUPERVISED LEARNING OUTPUTS
    #################################
    
    
    # Training variables
    supervised_learning_vars <- names(select(scores_df, 
                                             -matches("[A,H]Q[1-4]"),
                                             -matches("[A,H]OT2?"),
                                             -matches("[A,H]Final"),
                                             -startTime,
                                             -duration,
                                             -isFavoriteTheWinner,
                                             -spread_diff,
                                             -homeWin,
                                             -awayWin,
                                             -starts_with("season"),
                                             -HminusAScore
                                             ) %>% select_if(is.numeric))
    
    suggested_vars <- c("homeSpread", "homeYPA", "awayYPA", "homeYPC", "awayYPC", "homePlaysPerMinute", "awayPlaysPerMinute", "home3rdPct", "away3rdPct")

    
    ### CLASSIFICATION TREE STUFF
    # Tree reactive values
    treeRvs <- reactiveValues(predictionMadeThisModel = FALSE)
    
    updateSelectizeInput(session, "treeVars",
                         choices = supervised_learning_vars)
    
    # Tree: Use all variables button
    observeEvent(input$treeUseAllVarsBtn, {
        updateSelectizeInput(session, "treeVars",
                             selected = supervised_learning_vars)
    })
    
    # Random forest: Use suggested variables button
    observeEvent(input$treeUseSuggestedVarsBtn, {
        updateSelectizeInput(session, "treeVars",
                             selected = suggested_vars)
    })
    
    treeTrainParamsCp <- eventReactive(input$treeTrainButton, {
        input$treeCp
    })
    
    treeTrainParamsVars <- eventReactive(input$treeTrainButton, {
        input$treeVars
    })
    
    treeTrainParamsCvFolds <- eventReactive(input$treeTrainButton, {
        input$treeCvFolds
    })
    
    output$treeCvFoldsText <- renderText(treeTrainParamsCvFolds())
    
    output$treeCpText <- renderText({
        treeTrainParamsCp()
    })
    
    output$treeVarsText <- renderUI({
        tagList(
            lapply(treeTrainParamsVars(), span, class="badge")
        )
    })
    
    # Random forest model builder
    treeModel <- eventReactive(input$treeTrainButton, {
        print("Model building...")
        
        # Flag that predict was not yet made for this model
        treeRvs$predictionMadeThisModel <- FALSE
        
        get_test_accuracy <- function(model, testData) {
            # Make predictions
            preds <- predict(model, newdata = testData)
            
            # Create confusion matrix
            confusion_matrix <- table(preds, testData$homeWin)
            
            # Return accuracy rate
            sum(diag(confusion_matrix))/sum(confusion_matrix)
        }
        
        df <- scores_df %>% select(treeTrainParamsVars(), homeWin) %>% mutate(homeWin = as.factor(homeWin))
        
        # Set seed for reproducibility
        set.seed(1)
        
        # Referenced code from caret vignette: https://topepo.github.io/caret/data-splitting.html
        # Uses stratified random sampling
        trainIndex <- createDataPartition(df$homeWin, p = .8, 
                                          list = FALSE,
                                          times = 1) %>%
            as.vector()
        
        # Split into train and test
        scoresTrain <- df[trainIndex,]
        scoresTest  <- df[-trainIndex,]
        
        # Do cross validation
        train_control <- trainControl(method = "cv", number = input$treeCvFolds)
        
        # Build the random forest model using default tuning grid (mtry)
        random_forest_fit <- train(homeWin ~ ., data = scoresTrain, method = "rpart",
                                   trControl=train_control,
                                   tuneGrid = expand.grid(cp = c(input$treeCp)))
        
        # Return model and test accuracy
        print("Model building complete!")
        list(model = random_forest_fit, 
             test_accuracy = get_test_accuracy(random_forest_fit, scoresTest), 
             vars = names(df %>% select(-homeWin)))
    })
    
    output$treeModelCreated <- renderUI({
        withProgress(message = 'Training classification tree model',
                     detail = 'This may take a few minutes...', value = 0, {
                         incProgress(0.3)
                         treeModel()
                         incProgress(1)
                     }
        )
        tags$div()
    })
    
    output$treeCvAccuracyText <- renderText(treeModel()$model$results$Accuracy)
    output$treeTestAccuracyText <- renderText(treeModel()$test_accuracy)
    
    output$treePredictionForm <- renderUI({
        # Generate input
        generate_input <- function(varname) {
            column(width = 1, numericInput(paste0("treePredVar_", varname), varname, value = NULL))
        }
        
        fluidRow(
            lapply(treeModel()$vars,
                   generate_input)
        )
    })
    
    treePredictionInputDf <- eventReactive(input$treePredictButton, {
        # Flag that predict was made for this model
        treeRvs$predictionMadeThisModel <- TRUE
        
        # Get data from form
        print("Harvesting output")
        harvest_output <- function(varname) {
            input[[paste0("treePredVar_", varname)]]
        }
        
        df <- lapply(treeModel()$vars,
                     harvest_output)
        names(df) <- treeModel()$vars
        df
    })
    
    output$treePredictionMade <- renderUI({
        treePredictionInputDf()
        tags$div()
    })
    
    output$treePredictionInputTable <- renderTable({
        # Short circuit if the prediction wasn't from the current model
        if (!treeRvs$predictionMadeThisModel) {
            return()
        }
        treePredictionInputDf()
    })
    
    output$treePredictionOutput <- renderText({
        # Short circuit if the prediction wasn't from the current model
        if (!treeRvs$predictionMadeThisModel) {
            return()
        }
        
        prediction = predict(treeModel()$model, newdata = treePredictionInputDf())
        print(prediction)
        if (prediction == "TRUE") {
            return("Home team wins")
        } else {
            return("Home team does not win")
        }
    })
    
    # So that we can use it in conditionalPanel when it's not set or visible
    outputOptions(output, "treeModelCreated", suspendWhenHidden = FALSE)
    outputOptions(output, "treePredictionMade", suspendWhenHidden = FALSE)


    ### RANDOM FOREST STUFF
    # Random forest reactive values
    randomForestRvs <- reactiveValues(predictionMadeThisModel = FALSE)
        
    updateSelectizeInput(session, "randomForestVars",
                         choices = supervised_learning_vars)
    
    # Random forest: Use all variables button
    observeEvent(input$randomForestUseAllVarsBtn, {
        updateSelectizeInput(session, "randomForestVars",
                             selected = supervised_learning_vars)
    })
    
    # Random forest: Use suggested variables button
    observeEvent(input$randomForestUseSuggestedVarsBtn, {
        updateSelectizeInput(session, "randomForestVars",
                             selected = suggested_vars)
    })

    randomForestTrainParamsMtry <- eventReactive(input$randomForestTrainButton, {
        input$randomForestMtry
    })
    
    randomForestTrainParamsVars <- eventReactive(input$randomForestTrainButton, {
        input$randomForestVars
    })
    
    randomForestTrainParamsCvFolds <- eventReactive(input$randomForestTrainButton, {
        input$randomForestCvFolds
    })
    
    output$randomForestCvFoldsText <- renderText(randomForestTrainParamsCvFolds())
    
    output$randomForestMtryText <- renderText({
        randomForestTrainParamsMtry()
    })
    
    output$randomForestVarsText <- renderUI({
        tagList(
            lapply(randomForestTrainParamsVars(), span, class="badge")
        )
    })
    
    # Random forest model builder
    randomForestModel <- eventReactive(input$randomForestTrainButton, {
        print("Model building...")
        
        # Flag that predict was not yet made for this model
        randomForestRvs$predictionMadeThisModel <- FALSE
        
        get_test_accuracy <- function(model, testData) {
            # Make predictions
            preds <- predict(model, newdata = testData)
            
            # Create confusion matrix
            confusion_matrix <- table(preds, testData$homeWin)
            
            # Return accuracy rate
            sum(diag(confusion_matrix))/sum(confusion_matrix)
        }
        
        df <- scores_df %>% select(randomForestTrainParamsVars(), homeWin) %>% mutate(homeWin = as.factor(homeWin))

        # Set seed for reproducibility
        set.seed(1)
        
        # Referenced code from caret vignette: https://topepo.github.io/caret/data-splitting.html
        # Uses stratified random sampling
        trainIndex <- createDataPartition(df$homeWin, p = .8, 
                                          list = FALSE,
                                          times = 1) %>%
            as.vector()
        
        # Split into train and test
        scoresTrain <- df[trainIndex,]
        scoresTest  <- df[-trainIndex,]
        
        # Do repeated cross validation 3 times
        train_control <- trainControl(method = "cv", number = input$randomForestCvFolds)
        
        # Build the random forest model using default tuning grid (mtry)
        random_forest_fit <- train(homeWin ~ ., data = scoresTrain, method = "rf",
                                   trControl=train_control,
                                   tuneGrid = expand.grid(mtry = c(input$randomForestMtry)))
        
        
        # Accuracy (cross-validation)
        #random_forest_fit$results$Accuracy
        
        # Return model and test accuracy
        print("Model building complete!")
        list(model = random_forest_fit, 
             test_accuracy = get_test_accuracy(random_forest_fit, scoresTest), 
             vars = names(df %>% select(-homeWin)))
    })
    
    output$randomForestModelCreated <- renderUI({
        withProgress(message = 'Training random forest model',
                     detail = 'This may take a few minutes...', value = 0, {
                         incProgress(0.3)
                         randomForestModel()
                         incProgress(1)
                     }
        )
        tags$div()
    })
    
    output$randomForestCvAccuracyText <- renderText(randomForestModel()$model$results$Accuracy)
    output$randomForestTestAccuracyText <- renderText(randomForestModel()$test_accuracy)

    output$randomForestPredictionForm <- renderUI({
        # Generate input
        generate_input <- function(varname) {
            column(width = 1, numericInput(paste0("randomForestPredVar_", varname), varname, value = NULL))
        }
        
        fluidRow(
            lapply(randomForestModel()$vars,
                   generate_input)
        )
    })
    
    randomForestPredictionInputDf <- eventReactive(input$randomForestPredictButton, {
        # Flag that predict was made for this model
        randomForestRvs$predictionMadeThisModel <- TRUE
        
        # Get data from form
        print("Harvesting output")
        harvest_output <- function(varname) {
            input[[paste0("randomForestPredVar_", varname)]]
        }
        
        df <- lapply(randomForestModel()$vars,
               harvest_output)
        names(df) <- randomForestModel()$vars
        df
    })
    
    output$randomForestPredictionMade <- renderUI({
        randomForestPredictionInputDf()
        tags$div()
    })
    
    output$randomForestPredictionInputTable <- renderTable({
        # Short circuit if the prediction wasn't from the current model
        if (!randomForestRvs$predictionMadeThisModel) {
            return()
        }
        randomForestPredictionInputDf()
    })
    
    output$randomForestPredictionOutput <- renderText({
        # Short circuit if the prediction wasn't from the current model
        if (!randomForestRvs$predictionMadeThisModel) {
            return()
        }
        
        prediction = predict(randomForestModel()$model, newdata = randomForestPredictionInputDf())
        print(prediction)
        if (prediction == "TRUE") {
            return("Home team wins")
        } else {
            return("Home team does not win")
        }
    })
    
    # So that we can use it in conditionalPanel when it's not set or visible
    outputOptions(output, "randomForestModelCreated", suspendWhenHidden = FALSE)
    outputOptions(output, "randomForestPredictionMade", suspendWhenHidden = FALSE)
})
