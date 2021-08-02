#Project 3
#Zichang Xiang

#load packages
library(shinydashboard)
library(randomForest)
library(ggcorrplot)
#library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(GGally)
library(Metrics)
library(shiny)
library(caret)
library(DT)

options(warn = -1)

#read in and manipulate data
house <- read_csv("house.csv")
house <- house[,-1]
house$Class <- as.factor(ifelse(house$price<=50000, "Price <=50000", "Price >50000"))
house <- rename(house, Price = price, Lotsize = lotsize, Bedrooms= bedrooms, Bathrooms = bathrooms, 
                Stories = stories, Driveway = driveway, Recreation = recreation, Fullbase = fullbase, 
                Gasheat = gasheat, Aircon = aircon, Garage = garage, Prefer = prefer, Class = Class)
house1 <- house
house$Driveway <- as.factor(house$Driveway)
house$Recreation <- as.factor(house$Recreation)
house$Fullbase <- as.factor(house$Fullbase)
house$Gasheat <- as.factor(house$Gasheat)
house$Aircon <- as.factor(house$Aircon)
house$Prefer <- as.factor(house$Prefer)

# Define server logic
shinyServer(function(input, output, session) {
    
    getData <- reactive({
    col <- input$col
    row <- input$row
    newData <- subset(house[, col], Class == row)
    newData
  })
  
    getData1 <- reactive({
    var1 <- input$var1
    newData1 <- house[, var1]
    newData1
  })
    
###############################################################################################################################
    #data page
    
    #create table
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
    
    
###############################################################################################################################
    #explore page
    
    #create a new reactive variable for categorical variable tag
    getData2 <- reactive({
      newData <- house %>% filter(Stories == input$catrow)
    })
    
    #create a new reactive variable for quantitative variable tag
    getData3 <- reactive({
      newData <- house %>% filter(Prefer == input$qntrow)
    })
    
    output$cat1 <- renderTable(rownames = TRUE,{
      house <- getData2()

      if (input$catsum == "Frequency"){
      #frequency table
       as.data.frame.matrix(table(house$Class, house[[input$cat]]))
      } else if (input$catsum == "Proportion"){
      #proportion table
      a <- table(house$Class, house[[input$cat]])
      as.data.frame.matrix(prop.table(a))
      }
#      data.frame(no= c(48, 29), yes = c(110, 359), row.names = c("Price <=50000", "Price>50000"))
    }
)
    
    #react value when using the action button
    p <- reactiveValues(result = NULL)
    
    # output$catPlot1 <- renderPlot({
    #   house <- getData2()
    #   g <- ggplot(house, aes(x=.data[[input$cat]]))
    #   g + geom_bar(aes(fill = Class), position = "dodge") +
    #     labs(x=paste0(input$cat), title = paste0("Bar Plot of ", input$cat, " by Price"))
    # })
    # 
    # #download catPlot1
    # output$downloadcatPlot1 <- downloadHandler(
    #   filename = function(){paste0('Barplot of ', input$cat, '.png',sep='')},
    #   content = function(file){
    #     png(file)
    #     print(ggplot(getData2(), aes(x=.data[[input$cat]]))
    #            + geom_bar(aes(fill = Class), position = "dodge") +
    #             labs(x=paste0(input$cat), title = paste0("Bar Plot of ", input$cat, " by Price"))
    #       )
    #     dev.off()
    #   }
    # )
    
    output$catPlot1 <- renderPlot({
      house <- getData2()
      g <- ggplot(house, aes(x=.data[[input$cat]]))
      p$pic <- g + geom_bar(aes(fill = Class), position = "dodge") +
        labs(x=paste0(input$cat), title = paste0("Bar Plot of ", input$cat, " by Price"))
      p$pic
    })
    
    #download catPlot1
    output$downloadcatPlot1 <- downloadHandler(
      filename = function(){paste0('Barplot of ', input$cat, '.png',sep='')},
      content = function(file){
        png(file)
        print(p$pic)
        dev.off()
      }
    )
    
    # output$qnt1 <- renderTable(rownames = TRUE, {
    #   houseData <- getData1()
    #   a <- apply(houseData, MARGIN = 2, FUN = fivenum)
    #   a <- as.data.frame(a)
    #   rownames(a) <- c("Minimum", "Q1", "Median", "Q3", "Maximum")
    #   a
    # })
    
#     output$qntPlot1 <- renderPlot({
#       var <- input$var1
# #      correlation <- cor(house[,var])
# #      ggcorrplot(correlation, hc.order = TRUE, type = "lower",
# #                 outline.col = "white",
# #                 ggtheme = ggplot2::theme_gray)
#       ggcorr(house[,var], label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)
#     })
    
    #create qnt table
    output$qnt2 <- renderTable(rownames = FALSE,{
      house <- getData3()
      if (input$qntsum == "Central Tendency"){
      a <- house %>% summarise(Mean = mean(house[[input$qnt]]), Median = median(house[[input$qnt]]))
      a
      } else if (input$qntsum == "Spread"){
      a <- house %>% summarise(Variance = var(house[[input$qnt]]), Standard_Deviation = sd(house[[input$qnt]]),
                               IQR = IQR(house[[input$qnt]]), Minimum = min(house[[input$qnt]]), Maximum = max(house[[input$qnt]]),
                               Range = range(house[[input$qnt]]))
      a
      }
    })
    
    # #react value when using the action button
    # p <- reactiveValues(result = NULL)
    
    #create qnt boxplot
    output$qntPlot2 <- renderPlotly({
      fig <- getData3() %>% plot_ly(x = ~Class, y = ~.data[[input$qnt]], type = 'violin', box = list(visible = F),
                               meanline = list(visible = T), x0 = 'Class',color = I("#bebada"),
                               marker = list(line = list(width = 2,color = "#bebada"), symbol = 'line-ns'))
      p$fig <- fig %>% layout(yaxis = list(title = input$qnt, zeroline = F))
      p$fig
    })
    
    #download qnt boxplot
    output$saveqntPlot2 <- downloadHandler(
      filename = function(){paste0('Boxplot of ', input$qnt, '.png',sep='')},
      content = function(file){
        png(file)
        print(p$fig)
      }
    )
    
    
#    output$qntPlot3 <- renderPlot({
#      g <- ggplot(getData3(), aes(x=.data[[input$qnt]]))
#      g + geom_density(adjust = 0.5, alpha = 0.5, aes(fill = Class))
#    })
    
    
#    output$qntPlot4 <- renderPlot({
#      g <- ggplot(getData3(), aes(x=.data[[input$qnt]]))
#      g + geom_point(aes(x=.data[[input$qnt]], y = Price, color = Class), alpha = 0.6, size = 0.8, position = "jitter") 
#    })
    
    
    output$qntPlot3 <- renderPlot({
      if (input$qntplot == "Density Plot"){
      g <- ggplot(getData3(), aes(x=.data[[input$qnt]]))
      p$density <- g + geom_density(adjust = 0.5, alpha = 0.5, aes(fill = Class))+ labs(title = paste0("Density Plot of ", input$qnt, " by Price"))
      p$density
      } else if (input$qntplot == "Scatter Plot"){
      g <- ggplot(getData3(), aes(x=.data[[input$qnt]]))
      p$scatter <- g + geom_point(aes(x=.data[[input$qnt]], y = Price, color = Class), alpha = 0.6, size = 0.8, position = "jitter")+
        labs(title = paste0("Scatter Plot of ", input$qnt, " by Price"))
      p$scatter
      }
    })
    

    #download qntPlot1
    # output$downloadqntPlot1 <- downloadHandler(
    #   filename = function(){paste0('Correlation plot of ', input$qnt, '.png',sep='')},
    #   content = function(file){
    #     png(file)
    #     var <- input$var1
    #     correlation <- cor(house[,var])
    #     print(ggcorrplot(correlation, hc.order = TRUE, type = "lower",
    #           outline.col = "white",
    #           ggtheme = ggplot2::theme_gray))
    #     dev.off()
    #   }
    # )
    
    
    #download qntPlot3
#     output$downloadqntPlot3 <- downloadHandler(
#       filename = function(){paste0(input$qntplot, '.png',sep='')},
#       content = function(file){
#         png(file)
#         print(
#  #         if(input$qntplot == "Density Plot"){
#           ggplot(getData3(), aes(x=.data[[input$qnt]]))
#               + geom_density(adjust = 0.5, alpha = 0.5, aes(fill = Class))
# #          }else if (input$qntplot == "Scatter Plot"){
#          ggplot(house, aes(x=.data[[input$qnt]]))
#              + geom_point(aes(x=.data[[input$qnt]], y = Price, color = Class), alpha = 0.6, size = 0.8, position = "jitter")
#          }
#          )
#        dev.off()
    #   }
    # )
    
    #download qntPlot4
   # output$downloadqntPlot4 <- downloadHandler(
   #   filename = function(){paste0('Scatter plot of ', input$qnt, '.png',sep='')},
   #   content = function(file){
   #     png(file)
   #     print(ggplot(house, aes(x=.data[[input$qnt]]))
   #            + geom_point(aes(x=.data[[input$qnt]], y = Price, color = Class), alpha = 0.6, size = 0.8, position = "jitter") )
   #     dev.off()
   #   }
   # )
    
    
    
###############################################################################################################################
    # modeling page
    # model info tab
    
    # create function to update progress
    compute_data <- function(updateProgress = NULL) {
      # Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      for (i in 1:10) {
        Sys.sleep(0.25)
        
        # Compute new row of data
        new_row <- data.frame(x = rnorm(1), y = rnorm(1))
        
        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
          text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
          updateProgress(detail = text)
        }
        
        # Add the new row of data
        dat <- rbind(dat, new_row)
      }
      
      dat
    }
    
    #react value when using the action button
    m <- reactiveValues(result = NULL)
    
    observeEvent(input$fit, {
      
      split <- input$split
      
      # Create a Progress object
      progress <- shiny::Progress$new(style = "old")
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits
      on.exit(progress$close())
      
      # Create a closure to update progress.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      

      #update the progress indicator.
      compute_data(updateProgress)
      
      house <- house1[,-13]
      train <- sample(1:nrow(house), size = nrow(house)*split)
      test <- dplyr::setdiff(1:nrow(house), train)
      train <- house[train, ]
      test <- house[test, ]
      
      #MLR
      lmFit <- train(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))),
                     train,
                     method = "lm",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = input$cv, repeats = input$rep))

      m$lmFit <- summary(lmFit)

      lmtrainRMSE <- sqrt(mean(train$Price - predict(lmFit, train))^2)

      lmtestRMSE <- sqrt(mean(test$Price - predict(lmFit, test))^2)
      
      #regression tree
       set.seed(1)
       rtFit <- train(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))),
                      train,
                      method = "rpart",
                      trControl = trainControl(method = "cv", number = input$cv, repeats = input$rep),
                      preProcess = c("center", "scale"))

       m$rtFit <- rtFit
       
       rttrainRMSE <- sqrt(mean(train$Price - predict(rtFit, train))^2)
       
       rttestRMSE <- sqrt(mean(test$Price - predict(rtFit, test))^2)
       
       
      #random forest
      rfFit <- train(as.formula(paste("Price ~", paste(input$var2, collapse = "+"))),
                     train,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = input$cv, repeats = input$rep),
                     preProcess = c("center", "scale"))

      m$rfFit <- rfFit
      
      rftrainRMSE <- sqrt(mean(train$Price - predict(rfFit, train))^2)
      
      rftestRMSE <- sqrt(mean(test$Price - predict(rfFit, test))^2)
      
      m$results <- matrix(c(lmtrainRMSE, lmtestRMSE, rttrainRMSE, rttestRMSE, rftrainRMSE, rftestRMSE), nrow = 3, byrow = TRUE)
      
      colnames(m$results) <- c("trainRMSE", "testRMSE")
      rownames(m$results) <- c("MLR", "Regression Tree", "Random Forest")
      
      # Compute the new data, and pass in the updateProgress function so
      # that it can update the progress indicator.
      compute_data(updateProgress)
      
    })
    
    #output summary for MLR
    output$lm <- renderPrint({
      m$lmFit
    })
    
    #output summary for regression tree
    output$rt <- renderPrint({
    m$rtFit
    })
    
    #output summary for random forest
    output$rf <- renderPrint({
     m$rfFit
    })
    
    #output results for RMSE
    output$a <- renderPrint({
      m$results
    })
    
###############################################################################################################################
    #prediction page
    
    #react value when using the action button
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$pred, {
        split <- input$split
        house <- house[,-13]
        train <- sample(1:nrow(house), size = nrow(house)*split)
        test <- dplyr::setdiff(1:nrow(house), train)
        train <- house[train, ]
        test <- house[test, ]
        
        #remove the response variable from the data set
        testPred <- test[, -1]
        
        #create dataframe
        values <- data.frame(Lotsize = as.numeric(input$lot), Bedrooms = input$bedrooms, Bathrooms = input$bathrooms, Stories = input$stories, 
                             Driveway = input$driveway, Recreation = input$recreation, Fullbase = input$fullbase, Gasheat =  input$gasheat,
                             Aircon = input$aircon, Garage = input$garage, Prefer = input$prefer)
        
        testPred <- rbind(testPred, values)
        
        if (input$model == "Multiple Linear Regression"){
        #model fit
        lmFit <- train(Price ~ .,
                       train,
                       method = "lm",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = 10))

        a$result <- round(predict(lmFit, newdata = testPred[nrow(testPred),]), digits = 0)
        
        }else if (input$model == "Regression Tree"){
        treeFit <- tree(Price ~., data = train)
        a$result <- round(predict(treeFit, newdata = testPred[nrow(testPred),]), digits = 0)
        
        } else if (input$model == "Random Forest Model"){
          rfFit <- randomForest(Price ~ . , data = train)
          a$result <- round(predict(rfFit, newdata = testPred[nrow(testPred),]), digits = 0)
        }
    })
    
    
        output$value <- renderText({
          #display the prediction value
          paste(a$result)
        })
        




})

