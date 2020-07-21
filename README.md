# st-558-project-3

## Project Requirements

1. Your app should have multiple pages (tabs) to it. I don’t care if you use the built in tabs for shiny or a
package like shinydashboard - use the method you prefer.
  1. An information page that describes the data and abilities of the app
  2. A data exploration page where common numeric and graphical summaries can be created by the
user
  3. A page with either clustering (include a dendogram) or principal components analysis (include a
biplot) - again where the user can specify aspects of the algorithm
  4. A page for modeling - see below for details
  5. A page that allows the user to scroll through the data (or subset of data of interest)
2. You should have at least two different dynamic UI elements.
3. You should have a button that allows the user to save a plot they are viewing to a file.
4. You should have an option to allow the user to save the data currently being used for a plot (or when
they are looking at the data table) to a .csv (or other) file.
5. You should utilize the ability to click on a plot or select a region in some way.
6. You should include some type of math type (maybe an equation or just a special symbol you need to
use mathJax for).
7. You should include a link to something and some other formatted text.
8. Modeling
  1. (At least) two supervised learning models (feel free to branch out to things we didn’t discuss if
you’d like)
  2. You should give the user some functionality for choosing model settings (variables used, number of
trees, etc.) and for changing relevant output
  3. You should give the user a way to use the model for prediction (they should be able to select the
values of the predictors).

## College Football Drive Analyzer

### Tabs

#### Information Page (1.1, 6, 7)

* Data from https://collegefootballdata.com/ (link)
  * Quote what data is using italics with website description (7)
* App provides historical analysis of the 2019 season
  * Basic statistics for NCAA college football teams
  * Analysis of drives for NCAA college football teams
  * Prediction of drive statistics against a given opponent
* Formula for how Elo is calculated (6)

### Data Exploration Page (1.2, 3, 5)

All plots should have a button to save data or PNG to a file. (3)

All plots should use plotly. (5)

Title should change dynamically based on team selection.

* Basic Stats
  * Overall record (from season summary data)
  * Barplot of PA and PF by opponent (side-by-side) (from game summary data)
  * Boxplot of rushing vs. passing yards gained (from game summary data)
  * Boxplot of rushing vs. passing yards allowed (from game summary data)
* Drive Stats
  * Contingency table of drive results (from individual drive data)
  * Boxplot of elapsed time grouped by drive result (from individual drive data)
  * Numeric summary of yards per drive (from individual drive data)
  * Scatterplot of Opponent ELO vs. yards per drive (from individual drive data + season summary data for Elo)
  * Numeric summary of points per drive (from individual drive data + calculated column)
  * Numeric summary of drives per game (from individual drive data)
  * Numeric summary of possession time per game (from individual drive data)

### Unsupervised Learning (1.3)

Title should change dynamically based on team selection.

* PCA of drive data with biplot
  * User can select which variables to include?

### Modeling (1.4, 8.1, 8.2, 8.3)

* Models
  * Linear regression: Number of drives per game against an opponent (using opponent's defensive stats)
    * Options: lasso, (lasso) penalty, variables
  * Random forest: Number of drives per game against an opponent (using opponent's defensive stats)
    * Options: mtry, trees, variables
  * Give option to see confusion matrix in addition to output
* Prediction
  * Select teams (fills in values) OR provide values

### Data Table (1.5, 2, 4)

* Season summary data
* Game summary data by team (selector for team)
* Drive data by team and game (selector for team, dynamic game selector) (2)

All tables should be able to be saved to CSV with a button. (4)

Title should change dynamically based on selections. (2)
