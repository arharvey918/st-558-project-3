# st-558-project-3

## About

This is an R Shiny application created to do some rudimentary NFL data analysis. In the application, users can:

* **View** the data in tabular format
* **Explore** the data with interactive plots and create basic numeric and graphical summaries
* **Perform** unsupervised learning on the data using principal components analysis and visualize the results as a biplot
* **Predict** if a team will win a game after training different tree-based machine learning models

This project was created by Avy Harvey for ST 558 (Summer 2020).

## Quick Start

### R Packages

First, you'll need to have the following packages installed:

* shiny
* shinydashboard
* plotly
* DT
* tidyverse
* caret
* e1071
* randomForest

You can ensure they're installed with:

```r
install.packages(c("shiny", "shinydashboard", "plotly", "DT", "tidyverse", "caret", "e1071", "randomForest"))
```

### Run the App

Run the application from GitHub using `shiny`:

```r
shiny::runGitHub("arharvey918/st-558-project-3")
```

A web browser should appear with the application. Have fun!
