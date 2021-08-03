#Project 3
#Zichang Xiang

#load packages
library(shinydashboard)
library(randomForest)
library(ggcorrplot)
library(webshot)
library(ggplot2)
library(plotly)
library(GGally)
library(shiny)
library(caret)
library(readr)
library(tree)
library(DT)

options(warn = -1)

#read in and manipulate data
house <- read_csv("house.csv")
house <- house[,-1]
house$Class <- as.factor(ifelse(house$price<=50000, "Price <=50000", "Price >50000"))
house <- rename(house, Price = price, Lotsize = lotsize, Bedrooms=bedrooms, Bathrooms = bathrooms, 
                Stories = stories, Driveway = driveway, Recreation = recreation, Fullbase = fullbase, 
                Gasheat = gasheat, Aircon = aircon, Garage = garage, Prefer = prefer)

dashboardPage(
#    skin = "red",
    dashboardHeader(title = "House Price Analysis"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("info-circle")),
            menuItem("Data", tabName = "data", icon = icon("table")),
            menuItem("Data Exploration", tabName = "explore", icon = icon("bar-chart-o")),
            menuItem("Modeling", tabName = "model", icon = icon("line-chart", lib = "font-awesome"))
        )
    ),

    dashboardBody(
        tabItems(
            #about page
            tabItem(tabName = "about", 
                    fluidRow(
                        tabBox(title = NULL, width = 12,
                               tabPanel("Application Description", h3("About"), includeMarkdown("About.Rmd")),
                               tabPanel("Data Description", includeMarkdown("Data.Rmd"))
                              )
                        )
            ),
            
            #data page
            tabItem("data", 
                    box(checkboxGroupInput("col", h4("Select Columns"), 
                                       choices = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                   "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer", "Class"),
                                       selected = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                    "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer", "Class")),
                         h4("Select Rows"),
                         selectizeInput("row1", "Number of Rows", selected = "10", choices = 1:546),
                         downloadButton('downloadData','Download'),width = 2),
                    fluidPage(column(10,dataTableOutput("table")))
            ),
            
            #explore page
            tabItem(tabName = "explore",
                    fluidRow(
                        tabBox(
                            title = NULL, width = 12,
                            tabPanel("Categorical Variables",
                                     fluidRow(column(4,selectizeInput("cat", "Select Variables", selected = "Driveway", 
                                                    choices = c("Driveway", "Recreation", "Fullbase", "Gasheat", "Aircon", "Prefer"))),
                                              column(4,selectizeInput("catrow", "Select Stories", selected = "1", 
                                                                      choices = c("1", "2", "3", "4"))),
                                              column(4, selectizeInput("catsum", "Select Summary", selected = "Frequency", 
                                                              choices = c("Frequency", "Proportion"))),
                                              column(3, uiOutput("info1"), tableOutput("cat1")),
                                              column(9, h3("Plot"), plotOutput("catPlot1"), downloadButton('downloadcatPlot1','Download Plot')))),
                            
                            tabPanel("Quantitative Variables",
                                     fluidPage(
                                         column(2, checkboxGroupInput("var1", h4("Select Variables"),
                                                                     choices = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Garage"),
                                                                     selected = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Garage"))),
                                         column(5,align = "center", h4("Descriptive Summary Table"), tableOutput("qnt1")),
                                         column(5,align = "center", h4("Correlation Plot"), plotOutput("qntPlot1"),
                                                downloadButton('downloadqntPlot1','Download Plot')),
                                         column(12, br()),
                                         column(3, selectizeInput("qnt", "Select Variable", selected = "Lotsize", 
                                                    choices = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Garage"))),
                                         column(3, selectizeInput("qntrow", "Select Prefer", selected = "yes", choices = c("yes", "no"))),
                                         column(3, selectizeInput("qntsum", "Select Summary", selected = "Central Tendency", 
                                                              choices = c("Central Tendency", "Spread"))),
                                         column(3, selectizeInput("qntplot", "Select Plot", selected = "Density Plot", 
                                                              choices = c( "Density Plot", "Scatter Plot"))),
                                         uiOutput("info2"),
                                         column(12,tableOutput("qnt2")),
                                         br(),
                                         h3("Plots"),
                                         column(6, plotlyOutput("qntPlot2"), downloadButton('downloadqntPlot2','Download Plot')),
                                         column(6, plotOutput("qntPlot3"), downloadButton('downloadqntPlot3','Download Plot'))
                                     ))
                        )
                    )
            ),
            
            #tab model
            tabItem(tabName = "model", 
                    fluidRow(
                        tabBox(
                            title = NULL, width = 12,
                            tabPanel("Modeling Info", 
                                     fluidPage(
                                         h3("Multiple Linear Regression"),
                                         withMathJax(),
                                         helpText('Multiple linear regression (MLR) is a statistical method that uses multiple variables to 
                                         predict the outcome of the response variable. We use MLR to examine the relationship 
                                         between the predictor variables and the response variable. The model equation is presented below:'),
                                         helpText('$$y_i = \\beta_0 + \\beta_1 x_{i1} + \\beta_2 x_{i2} + \\cdots +\\beta_px_{ip} + \\epsilon $$'),
                                         helpText('where, for i = n observations:'),
                                         helpText('\\(y_i\\) = response variable'),
                                         helpText('\\(x_i\\) = predictor variables'),
                                         helpText('\\(\\beta_0 = \\) intercept'),
                                         helpText('\\(\\beta_p = \\) coefficient for each predictor variable'),
                                         helpText('\\(\\epsilon = \\) residual'),
                                         h4("Benefits: "),
                                         helpText('1. using MLR, we can easily examine the relationship between multiple predictors and the 
                                                  response variables, which can help us find which predictor variables has big influence on the response
                                                  variable.'),
                                         helpText('2. MLR can help us detect outliers in the data. For example, we have three predictor variables. 
                                                  Two of them have strong correlation with the response variable, while the third one does not. 
                                                  This predictor variable may have outliers that can explain why it does not have strong correlation 
                                                  with the response variable.'),
                                         h4("Drawbacks:"),
                                         helpText('The data we used to fit the MLR can affect the inference we made based on the result of MLR. For example,
                                                  if the data we use cannot represent the population we are interented in, then the conclusion we made may not
                                                  reflect the true condition of the population.'),
                                         h3("Regression Tree"),
                                         helpText('Regression Tree is a statistical method used to predict a continuous response. It is one kind of decision tree
                                                  learning, which uses decision tree as a predictive model to split up predictor space into regions and make
                                                  different predictions for each region. For a given region, we usually use mean of observations as prediction.'),
                                         helpText('For every possible value of each predictor, we try to minimize the Residual Sum of Squares.$$ R_1(j,s) =
                                                  \\{x|x_j < s\\}\\ and \\ R_2(j,s)=\\{x|x_j \\ge s\\}$$'),
                                         helpText('We seek the value of j and s that minimize the equation'),
                                         helpText('$$\\sum_{i:x_i \\in R_1(j,s)} (y_i -\\bar y_{R_1})^2 + \\sum_{i:x_i \\in R_2(j,s)}(y_i -\\bar y _{R_2})^2 $$'),
                                         h4('Benefits:'),
                                         helpText('1. It is easy to understand and the output is easy to interpret'),
                                         helpText('2. We do not need to scale the predictors'),
                                         helpText('3. We do not need to make any assumption before fitting the regression tree model'),
                                         helpText('4. The regression tree can select variables automatically. We do not need to do that ourselves.'),
                                         h4('Drawbacks:'),
                                         helpText('1.The regression tree model is very sensitive to the change of data. A small changes in the data will 
                                                  largely influence the tree.'),
                                         helpText('2.There is no optimal algorithm. It is necessary for us to use the greedy algorithm.'),
                                         helpText('3.The tree model can be very complex. Thus, we need to prune the tree to make it less complex and more 
                                                  accurate in prediction.'),
                                         h3("Random Forest Model"),
                                         helpText('Random Forest is a statistical predictive model that uses subset of multiple trees from bootstrap samples
                                         and averages across these trees to get a better prediction. We can still examine variable importance, but the results
                                                  are often hard to interpret, since we average all the fitted trees.'),
                                         helpText('We usually use m=p/3 randomly selected predictors. If m=p then we will have bagging trees. 
                                                  We can determine m through OOB error.'),
                                         helpText('We use the mean squared error (MSE) to determine how the data branches from each node.
                                         $$MSE = \\frac{1}{N}\\sum ^N _{i=1}(f_i-y_i)^2 $$ Where N is the number of data points'),
                                         helpText('\\(f_i\\) is the fitted value'),
                                         helpText('\\(y_i\\) is the actual value for data point i'),
                                         helpText('We use this formula to calculates the distance between each node and the predicted
                                                  value. The closest branch will be the best decision for our forest.'),
                                         h4("Benefits:"),
                                         helpText('1. We reduce overfitting problem and improve the accuracy of our prediction, 
                                                  since we average over multiple tree fits and decreases the variance of a single tree fit'),
                                         helpText('2. Since we use randomly selected subset of predictors, 
                                                  the fitted trees will be less correlated between each 
                                                  other, which can help us largely reduce variance from aggregation.'),
                                         helpText('3. We do not need to consider scale the predictors.'),
                                         helpText('4. The random forest model can missing values automatically.'),
                                         helpText('5. It can be used for both the categorical and continuous response.'),
                                         h4("Drawbacks:"),
                                         helpText('1. The result is usually hard to interpret, because we average over multiple tree fits.'),
                                         helpText('2. It usually require much more time to train than regression tree, because it generates multiple trees.'),
                                         h3("Reference"),
                                         helpText('1. Adam, H (2021, Mar 30). Multiple Linear Regression (MLR). Investopedia. 
                                                  https://www.investopedia.com/terms/m/mlr.asp'),
                                         helpText('2. Jamie, S (2021, August 2) Decision tree learning. Wikipedia. 
                                                  https://en.wikipedia.org/wiki/Decision_tree_learning'),
                                         helpText('3. Hooman, M (2021, July 12). Random Forest. Wikipedia. https://en.wikipedia.org/wiki/Random_forest')
                                     )),
                            
                            tabPanel("Model Fitting", 
                                     fluidRow(column(4, 
                                                     h3("Split Data"),
                                                     sliderInput("split", "Train/Test Proportion", min = 0, max = 1, value = 0.7, step = 0.1),
                                                     h3("Cross Validation"),
                                                     sliderInput("cv", "Number of folds", min = 0, max = 10, value = 3, step = 1),
                                                     h3("Repeat"),
                                                     sliderInput("rep", "Number", min = 0, max = 5, value = 0, step = 1),
                                                     checkboxGroupInput("var2", h3("Select Variables"), 
                                                        choices = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                                    "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer"),
                                                        selected = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway")),
                                                     actionButton("fit","Fit Models"),
                                                     br(),
                                                     verbatimTextOutput("a")),
                                              column(8, h3("Multiple Linear Regression"), 
                                                     verbatimTextOutput("lm"),
                                                     h3("Regression Tree"), verbatimTextOutput("rt"),
                                                     h3("Random Forest Model"), verbatimTextOutput("rf", placeholder = TRUE))
                                             )),
                            
                            tabPanel("Prediction",
                                     fluidPage(
                                       column(12, 
                                              h3("Select Model"),
                                              selectizeInput("model", "Model", selected = "Multiple Linear Regression",
                                                                 choices = c("Multiple Linear Regression", "Regression Tree", "Random Forest Model"))),
                                       column(12, h3("Categorical Variables")),
                                       column(3, radioButtons("driveway", "Driveway", selected = "yes", choices = c("yes", "no"))),
                                       column(3, radioButtons("recreation", "Recreation", selected = "yes", choices = c("yes", "no"))),
                                       column(3, radioButtons("fullbase", "Fullbase", selected = "yes", choices = c("yes", "no"))),
                                       column(3, radioButtons("gasheat", "Gasheat", selected = "yes", choices = c("yes", "no"))),
                                       column(3, radioButtons("aircon", "Aircon", selected = "yes", choices = c("yes", "no"))),
                                       column(3, radioButtons("prefer", "Prefer", selected = "yes", choices = c("yes", "no"))),
                                       column(6, ),
                                       column(12, h3("Numerical Variables")),
                                       column(3, selectInput("lot", "Lotsize", selected = "1650", choices = levels(as.factor(house$Lotsize)))),
                                       column(3, sliderInput("bedrooms", "Bedrooms", min = 1, max = 4, value = 1, step = 1)),
                                       column(3, sliderInput("bathrooms", "Bathrooms", min = 1, max = 4, value = 1, step = 1)),
                                       column(3, sliderInput("stories", "Stories", min = 1, max = 4, value = 1, step = 1)),
                                       column(12, sliderInput("garage", "Garage", min = 0, max = 3, value = 0, step = 1)),
                                       column(12, numericInput("maxBedrooms", label = "Set Maximum Number of Bedrooms", value = 1, min = 1, max = 6)),
                                       column(12, h3("Prediction Result")),
                                       column(12, h5("House Price:")),
                                       column(3, verbatimTextOutput("value", placeholder = TRUE)),
                                       column(9, ),
                                       column(12, actionButton('pred', 'Predict', icon = icon('calculator')))
                                     )

                                     )
                        )
                    )
            
            )
        )
    )
)
