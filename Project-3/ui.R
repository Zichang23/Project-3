#Project 3
#Zichang Xiang

#load packages
library(shinydashboard)
library(shiny)

dashboardPage(
#    skin = "red",
    dashboardHeader(title = "Dynamic sidebar"),
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
            # First tab content
            tabItem(tabName = "explore",
                    fluidRow(
                        box(title = "Controls", 
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        ),
                        box(plotOutput("plot1", height = 250))
                    )
            ),
            # Second tab content
            tabItem(tabName = "about", h2("About page")
            ),
            tabItem(tabName = "data", h2("Data page")
            ),
            tabItem(tabName = "explore", h2("Data Exploration page")
            ),
            tabItem(tabName = "model", h2("Modeling page")
            )
        )
    )
)
