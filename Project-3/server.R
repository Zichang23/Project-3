#Project 3
#Zichang Xiang

#load packages
library(shinydashboard)
library(randomForest)
library(ggcorrplot)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)
library(Metrics)
library(shiny)
library(caret)
library(DT)

#read in and manipulate data
house <- read_csv("house.csv")
house <- house[,-1]
house$Class <- as.factor(ifelse(house$price<=50000, "Price <=50000", "Price >50000"))
house <- rename(house, Price = price, Lotsize = lotsize, Bedrooms=bedrooms, Bathrooms = bathrooms, 
                Stories = stories, Driveway = driveway, Recreation = recreation, Fullbase = fullbase, 
                Gasheat = gasheat, Aircon = aircon, Garage = garage, Prefer = prefer, Class = Class)
house$Driveway <- as.factor(house$Driveway)
house$Recreation <- as.factor(house$Recreation)
house$Fullbase <- as.factor(house$Fullbase)
house$Gasheat <- as.factor(house$Gasheat)
house$Aircon <- as.factor(house$Aircon)
house$Prefer <- as.factor(house$Prefer)

# Define server logic
shinyServer(function(input, output, session) {
    
    getData <- reactive({
    var <- input$var
    newData <- house[, var]
    newData
  })
  
    getData1 <- reactive({
    var1 <- input$var1
    newData1 <- house[, var1]
    newData1
  })
    
  
    
    #data page: create table
    output$table <- DT::renderDataTable(rownames = FALSE, 
#                                        house[,input$var]
                                        getData()
                                        )
    
    #download table
    output$downloadData <- downloadHandler(
      filename = function(){'House.csv'},
      content = function(file){
        write.csv(getData(), file, row.names = FALSE)
      }
    )

    #explore page
    output$cat1 <- renderTable(
      rownames = TRUE,
#      {
      as.data.frame.matrix(table(house$Class, house[[input$cat]]))
#      data.frame(no= c(48, 29), yes = c(110, 359), row.names = c("Price <=50000", "Price>50000"))
#    }    
)
    
    output$catPlot1 <- renderPlot({
      g <- ggplot(house, aes(x=.data[[input$cat]]))
      g + geom_bar(aes(fill = Class), position = "dodge") + 
        labs(x=paste0(input$cat), title = paste0("Bar Plot of ", input$cat, " by Price"))
    })
    
    #download catPlot1
    output$downloadcatPlot1 <- downloadHandler(
      filename = function(){paste0('Barplot of ', input$cat, '.png',sep='')},
      content = function(file){
        png(file)
        print(ggplot(house, aes(x=.data[[input$cat]]))
               + geom_bar(aes(fill = Class), position = "dodge") + 
                labs(x=paste0(input$cat), title = paste0("Bar Plot of ", input$cat, " by Price")))
        dev.off()
      }
    )
    
    output$qnt1 <- renderTable(rownames = TRUE, {
      houseData <- getData1()
      a <- apply(houseData, MARGIN = 2, FUN = fivenum)
      a <- as.data.frame(a)
      rownames(a) <- c("Minimum", "Q1", "Median", "Q3", "Maximum")
      a
    })
    
    output$qntPlot1 <- renderPlot({
      var <- input$var1
#      correlation <- cor(house[,var])
#      ggcorrplot(correlation, hc.order = TRUE, type = "lower",
#                 outline.col = "white",
#                 ggtheme = ggplot2::theme_gray)
      ggcorr(house[,var], label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)
    })
    
    output$qntPlot2 <- renderPlotly({
      fig <- house %>% plot_ly(x = ~Class, y = ~.data[[input$qnt]], type = 'violin', box = list(visible = F), 
                               meanline = list(visible = T), x0 = 'Class',color = I("#bebada"),
                               marker = list(line = list(width = 2,color = "#bebada"), symbol = 'line-ns')) 
      fig <- fig %>% layout(yaxis = list(title = "", zeroline = F))
      fig
    })
    
    output$qntPlot3 <- renderPlot({
      g <- ggplot(house, aes(x=.data[[input$qnt]]))
      g + geom_density(adjust = 0.5, alpha = 0.5, aes(fill = Class))
    })
    
    
    output$qntPlot4 <- renderPlot({
      g <- ggplot(house, aes(x=.data[[input$qnt]]))
      g + geom_point(aes(x=.data[[input$qnt]], y = Price, color = Class), alpha = 0.6, size = 0.8, position = "jitter") 
    })
    
    
    #download qntPlot1
    output$downloadqntPlot1 <- downloadHandler(
      filename = function(){paste0('Correlation plot of ', input$qnt, '.png',sep='')},
      content = function(file){
        png(file)
        var <- input$var1
        correlation <- cor(house[,var])
        print(ggcorrplot(correlation, hc.order = TRUE, type = "lower",
              outline.col = "white",
              ggtheme = ggplot2::theme_gray))
        dev.off()
      }
    )
    
    #download qntPlot2
    output$downloadqntPlot2 <- downloadHandler(
      filename = function(){paste0('Boxplot of ', input$qnt, '.png',sep='')},
      content = function(file){
        png(file)
        print(fig <- house %>% plot_ly(x = ~Class, y = ~.data[[input$qnt]], type = 'violin', box = list(visible = F), 
                                       meanline = list(visible = T), x0 = 'Class',color = I("#bebada"),
                                       marker = list(line = list(width = 2,color = "#bebada"), symbol = 'line-ns')), 
              fig <- fig %>% layout(yaxis = list(title = "", zeroline = F)),
              fig)
        dev.off()
      }
    )
    
    #download qntPlot3
    output$downloadqntPlot3 <- downloadHandler(
      filename = function(){paste0('Density plot of ', input$qnt, '.png',sep='')},
      content = function(file){
        png(file)
        print(ggplot(house, aes(x=.data[[input$qnt]]))
              + geom_density(adjust = 0.5, alpha = 0.5, aes(fill = Class)))
        dev.off()
      }
    )
    
    #download qntPlot4
    output$downloadqntPlot4 <- downloadHandler(
      filename = function(){paste0('Scatter plot of ', input$qnt, '.png',sep='')},
      content = function(file){
        png(file)
        print(ggplot(house, aes(x=.data[[input$qnt]]))
               + geom_point(aes(x=.data[[input$qnt]], y = Price, color = Class), alpha = 0.6, size = 0.8, position = "jitter") )
        dev.off()
      }
    )
    
    
    # modeling page
    
    #fit multiple linear regression model
    output$lm <- renderPrint({
      split <- input$split
      house <- house[,-13]
      train <- sample(1:nrow(house), size = nrow(house)*split)
      test <- dplyr::setdiff(1:nrow(house), train)
      train <- house[train, ]
      test <- house[test, ]
      
      lmFit <- lm(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))), train)
      
      #train RMSE
      trainRMSE <- sqrt(mean(train$Price - predict(lmFit, train))^2)
      
      #test RMSE
      testRMSE <- sqrt(mean(test$Price - predict(lmFit, test))^2)
      
      list(trainRMSE = trainRMSE, testRMSE = testRMSE, lmFit = summary(lmFit))
    })
    
    #fit regression tree
    output$rt <- renderPrint({
      split <- input$split
      house <- house[,-13]
      train <- sample(1:nrow(house), size = nrow(house)*split)
      test <- dplyr::setdiff(1:nrow(house), train)
      train <- house[train, ]
      test <- house[test, ]
      
      set.seed(1)
      treeFit <- tree(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))), data = train)
      
      #train RMSE
      trainRMSE <- sqrt(mean(train$Price - predict(treeFit, train))^2)
      
      #test RMSE
      testRMSE <- sqrt(mean(test$Price - predict(treeFit, test))^2)

      list(trainRMSE = trainRMSE, testRMSE = testRMSE, treeFit = summary(treeFit))
    })
    
    #fit random forest model
    output$rf <- renderPrint({
      split <- input$split
      house <- house[,-13]
      train <- sample(1:nrow(house), size = nrow(house)*split)
      test <- dplyr::setdiff(1:nrow(house), train)
      train <- house[train, ]
      test <- house[test, ]
      
#      rfFit <- train(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))), 
#                     data = train, 
#                     method = "rf",
#                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
#                     preProcess = c("center", "scale"))
#      summary(rfFit$finalModel)
      set.seed(1)
      rfFit <- randomForest(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))), 
                            data = train)
      #train RMSE
      trainRMSE <- sqrt(mean(train$Price - predict(rfFit, train))^2)
      
      #test RMSE
      testRMSE <- sqrt(mean(test$Price - predict(rfFit, test))^2)

      list(trainRMSE = trainRMSE, testRMSE = testRMSE, rfFit = rfFit, importance = rfFit$importance)
    })
    
    #prediction page
    output$title <- renderUI({
      if(input$pred == "Multiple Linear Regression"){
        h3("Multiple Linear Regression")
      } else if(input$pred == "Regression Tree") {
        h3("Regression Tree")
      } else if(input$pred == "Random Forest Model") {
        h3("Random Forest Model")
      }
    })
    
    output$Pred <- renderPrint({
      split <- input$split
      house <- house[,-13]
      train <- sample(1:nrow(house), size = nrow(house)*split)
      test <- dplyr::setdiff(1:nrow(house), train)
      train <- house[train, ]
      test <- house[test, ]
      
      if (input$pred == "Multiple Linear Regression"){
        lmFit <- lm(as.formula(paste("Price ~", paste(input$var3, collapse = "+"))), train)
        lmPred <- predict(lmFit, test, interval = "confidence")
        return(lmPred)
      } else if (input$pred == "Regression Tree"){
        set.seed(1)
        treeFit <- tree(as.formula(paste("Price ~", paste(input$var3, collapse = "+"))), train)
        rtPred <- predict(treeFit, test, interval = "confidence")
        rtPred
      } else if (input$pred == "Random Forest Model"){
        set.seed(1)
        rfFit <- randomForest(as.formula(paste("Price ~", paste(input$var3, collapse = "+"))), train)
        rfPred <- predict(rfFit, test, interval = "confidence")
        rfPred
      }
      
    })



})

