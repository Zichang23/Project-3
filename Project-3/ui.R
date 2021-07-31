#Project 3
#Zichang Xiang

#load packages
library(shinydashboard)
library(tidyverse)
library(markdown)
library(ggcorrplot)
library(ggplot2)
library(plotly)
library(shiny)
library(caret)
library(DT)


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
                    box(checkboxGroupInput("var", h4("Select Variables"), 
                                       choices = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                   "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer", "Class"),
                                       selected = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                    "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer", "Class")),
                        downloadButton('downloadData','Download'),
                        width = 2),
                    fluidPage(
                          column(10,dataTableOutput("table"))
                        )
            ),
            
            #explore page
            tabItem(tabName = "explore",
                    fluidRow(
                        tabBox(
                            title = NULL, width = 12,
                            tabPanel("Categorical Variables",
                                     selectizeInput("cat", "Select Variables", selected = "Driveway", 
                                                    choices = c("Driveway", "Recreation", "Fullbase", "Gasheat", "Aircon", "Prefer")),
                                     plotOutput("catPlot1"),
                                     downloadButton('downloadcatPlot1','Download Plot'),
                                     tableOutput("cat1")),
                            tabPanel("Quantitative Variables",
                                     column(2, checkboxGroupInput("var1", h4("Select Variables"), 
                                                        choices = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Garage"),
                                                        selected = c("Price", "Lotsize", "Bedrooms", "Bathrooms", "Stories", "Garage"))),
                                     column(5, tableOutput("qnt1")),
                                     column(5, plotOutput("qntPlot1"),downloadButton('downloadqntPlot1','Download Plot')),

                                     selectizeInput("qnt", "Select Variable", selected = "Price", 
                                                    choices = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Garage")),
                                     plotlyOutput("qntPlot2"),
                                     downloadButton('downloadqntPlot2','Download Plot'),
                                     plotOutput("qntPlot3"),
                                     downloadButton('downloadqntPlot3','Download Plot'),
                                     plotOutput("qntPlot4"),
                                     downloadButton('downloadqntPlot4','Download Plot')
                                     )
                        )
                    )
            ),
            
            #tab model
            tabItem(tabName = "model", 
                    fluidRow(
                        tabBox(
                            title = NULL, width = 12,
                            tabPanel("Modeling Info", "Modeling Info"),
                            tabPanel("Model Fitting", 
                                     fluidRow(column(4, 
                                     sliderInput("split", "Split Data (train/test proportion)",
                                                 min = 0, max = 1, value = 0.7, step = 0.1),
                                     checkboxGroupInput("var2", "Select Variables", 
                                                        choices = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                                    "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer"),
                                                        selected = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                                     "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer")),
                                     actionButton("fit","Fit Models")),
                                     column(8, h3("Multiple Linear Regression"), verbatimTextOutput("lm"),
                                         h3("Regression Tree"), verbatimTextOutput("rt"),
                                         h3("Random Forest Model"), verbatimTextOutput("rf")))),
                            tabPanel("Prediction", 
                                     fluidRow(column(4,
                                     selectizeInput("pred", "Select Model", selected = "Multiple Linear Regression", 
                                                    choices = c("Multiple Linear Regression", "Regression Tree", "Random Forest Model")),
                                     h3("Select Values"),
                                     sliderInput("lot", "Lotsize",
                                                 min = 1650, max = 16200, value = 1650, step = 10),
                                     sliderInput("bedrooms", "Bedrooms",
                                                 min = 1, max = 6, value = 1, step = 1),
                                     sliderInput("bathrooms", "Bathrooms",
                                                 min = 1, max = 4, value = 1, step = 1),
                                     sliderInput("stories", "Stories",
                                                 min = 1, max = 4, value = 1, step = 1),
                                     checkboxGroupInput("var3", "Select Variables", 
                                                        choices = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                                    "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer"),
                                                        selected = c("Lotsize", "Bedrooms", "Bathrooms", "Stories", "Driveway",
                                                                     "Recreation", "Fullbase", "Gasheat", "Aircon", "Garage", "Prefer"))),
                                     column(8,uiOutput("title"),
                                            verbatimTextOutput("Pred")))
                                     )
                        )
                    )
            
            )
        )
    )
)
