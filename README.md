# Project-3

#### Description of the app and its purpose

Description: this shiny app is based on shinydashboard. It has four pages, including `About`, `Data`, `Data Exploration`, and `Modeling`. Users can use this app to view and download data and plots, choose models to fit data, and predict house price based on different values of the predictors.

Purpose: this application is aimed to explore the house price data and find factors that have great influence on the house price.

#### List of packages needed to run the app
The packages include `shiny`, `shinydashboard`, `readr`, `ggcorrplot`, `ggplot2`, `GGally`, `plotly`, `caret`, `webshot`, `PhantomJS`, `tree`, and `DT`.

#### Code to install all the packages used
`install.packages(c("shiny", "shinydashboard", "readr", "ggcorrplot", "ggplot2", "GGally",  "plotly", "caret", "webshot", "tree", "DT"))`

`webshot::install_phantomjs()`

#### Code to run the app
`shiny::runGitHub("Project-3", "Zichang23", subdir = "Project-3", ref = "main")`

