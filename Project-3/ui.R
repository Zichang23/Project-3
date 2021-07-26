#Project 3
#Zichang Xiang

#load packages
library(shinydashboard)
library(markdown)
library(shiny)

dashboardPage(
#    skin = "red",
    dashboardHeader(title = "Diabetes Data Analysis"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
#            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("info-circle")),
            menuItem("Data", tabName = "data", icon = icon("table")),
            menuItem("Data Exploration", tabName = "explore", icon = icon("bar-chart-o")),
            menuItem("Modeling", tabName = "model", icon = icon("line-chart", lib = "font-awesome"))
        )
#        ,
#        sidebarMenu(
#            menuItemOutput("menuitem")
#        )
    ),
    dashboardBody(
        tabItems(
            # tab about
            tabItem(tabName = "about", 
                    fluidRow(
                        tabBox(
                            title = NULL, width = 12,
                            tabPanel("Application Description",
                                     includeHTML("About.html")),
                            tabPanel("Data Description", includeHTML("Data.html"))
                        )
                        )
            ),
            
            #tab data
            tabItem(tabName = "data", h2("Data page")
            ),
            
            #tab explore
            tabItem(tabName = "explore",
                    fluidRow(
                        box(title = "Controls", 
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        ),
                        box(plotOutput("plot1", height = 250))
                    )
            ),
            
            #tab model
            tabItem(tabName = "model", 
                    fluidRow(
                        tabBox(
                            title = NULL, width = 12,
                            tabPanel("Modeling Info", "Modeling Info"),
                            tabPanel("Model Fitting", "Model Fitting"),
                            tabPanel("Prediction", "Prediction")
                        )
                    )
            
            )
        )
    )
)
